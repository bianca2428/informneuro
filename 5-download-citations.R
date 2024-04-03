library(tidyverse)
library(openalexR)
library(lubridate)
library(testthat)
library(pubmedtk)
library(httr)
library(jsonlite)

## Where to write the results
citationsfile <- "data/citations.csv"
nocitationsfile <- "data/no-citations.csv"

## Pubmed API key
pmak <- readLines("data/pubmed_api_key.txt")
scak <- readLines("data/scite_api_key.txt")

## Define queries for animal and clinical studies
animals_query <- readLines("data/animals-query.txt") ## 10.1258/la.2010.009117
clinical_query <- "clinicaltrial[Filter] OR (Clinical Trial[Publication Type])"

## Read in the NCT numbers and DOIs from disk
## We also downloaded the data for the pilot project at the same time
pilot <- read_csv("data/pilot.csv") %>%
    mutate(sample = "pilot")
neuro <- read_csv("data/neuro-sample.csv") %>%
    mutate(sample = "neuro")

trials <- pilot %>%
    bind_rows(neuro)

## Downloading the OpenAlex data for a column of DOIs
works_from_dois <- oa_fetch(
    entity = "works",
    doi = unique(trials$doi)
)

test_that(
    "There are no duplicate DOIs in the result from the query to the API",
    {
        
        ## Check for duplicate dois
        works_from_dois$dup <- duplicated(works_from_dois$doi)

        ## Should be 0
        n_dups <- sum(works_from_dois$dup)

        ## Get rid of unneccesary column
        works_from_dois$dup <- NULL

        expect_equal(
            n_dups,
            0
        )
        
    }
    
)

get_citations_from_scite <- function (ak, doi, offset, limit) {

    if (! grepl("^10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+$", doi)) {
        stop("DOI is not well-formed")
    }
    
    out <- tryCatch({

        response <- GET(
            paste0(
                "https://api.scite.ai/api_partner/citations/citing/",
                doi,
                "?offset=",
                offset,
                "&limit=",
                limit
            ),
            add_headers(
                c(
                    'Authorization' = paste("Bearer", ak),
                    'Content-Type' = 'application/json',
                    'User-Agent' = 'insomnia/8.2.0'
                )
            )
        )

        if (response$status == 200) {

            rc <- content(response, "text", encoding="UTF-8") %>%
                fromJSON()
            
            return (rc)
            
        } else {

            stop(response$status)
            
        }
        
    },
    error = function(cond) {
        message(cond)
        return(NA)
    })

}

## Convert the DOIs to lowercase because that's what they got at OA
trials <- trials %>%
    mutate(
        doi = tolower(paste0("https://doi.org/", doi))
    )

## Join the original list of NCT's to the downloaded data
trials_with_ids <- left_join(
    trials,
    works_from_dois,
    by="doi"
)

## Trim the DOIs and OpenAlex ids to remove the URL stuff at the
## beginning and also rename the `id` column to be more descriptive
trials_with_ids <- trials_with_ids %>%
    mutate(
        doi = substr(doi, 17, 1000000L)
    ) %>%
    mutate(
        id = substr(id, 22, 1000000L)
    ) %>%
    rename(openalex_id = id)

readfiles <- TRUE

for (id in unique(trials_with_ids$doi)) {

    ## Read file if it exists, write a new one if it doesn't
    if (file.exists(citationsfile)) {
        if (readfiles) {
            citations <- read_csv(citationsfile, col_types=cols())
            nocitations <- read_csv(nocitationsfile, col_types=cols())
        }
    } else {    
        citations <- tribble(
            ~trial_doi,
            ~trial_pmid,
            ~trial_openalex_id,
            ~trial_title,
            ~trial_pubdate,
            ~citing_doi,
            ~citing_pmid,
            ~citing_openalex_id,
            ~citing_title,
            ~citing_pubdate,
            ~citing_is_animal,
            ~citing_is_clinical,
            ~cite_type
        )

        citations %>%
            write_csv(citationsfile)

        nocitations <- tribble(
            ~trial_doi,
            ~trial_pmid,
            ~trial_title,
            ~trial_pubdate
        )

        nocitations %>%
            write_csv(nocitationsfile)

        readfiles <- TRUE
    }

    ## Only do stuff in this loop if the DOI hasn't been processed yet
    if (! (id %in% citations$trial_doi | id %in% nocitations$trial_doi)) {

        message(
            paste("Processing DOI:", id)
        )

        trial_doi <- id

        if (nrow(filter(trials_with_ids, doi == id & ! is.na(openalex_id))) > 0) {

            trial_openalex_id <- trials_with_ids %>%
                filter(doi == id) %>%
                slice_head() %>%
                pull(openalex_id)

            trial_pmid <- trials_with_ids %>%
                filter(doi == id) %>%
                slice_head() %>%
                pull(ids) %>%
                purrr::map("pmid") %>%
                as.character() %>%
                substr(33, 1000000L)
            
            trial_title <- trials_with_ids %>%
                filter(doi == id) %>%
                slice_head() %>%
                pull(display_name)

            trial_pubdate <- trials_with_ids %>%
                filter(doi == id) %>%
                slice_head() %>%
                pull(publication_date)

            ## Only download citations less than five years after the
            ## trial's publication date
            max_pubdate <- ymd(trial_pubdate) + years(5)

            ## Download citations of the work in question before
            ## `max_pubdate`
            trial_cites <- NA
            trial_cites <- oa_fetch(
                entity="works",
                cites=trial_openalex_id,
                to_publication_date=paste0(max_pubdate)
            )
            
        } else {
            trial_cites <- NA
        }

        if (is.data.frame(trial_cites)) {

            ## Reformat DOI to remove the URL stuff
            trial_cites <- trial_cites %>%
                mutate(
                    citing_pmid = substr(
                        as.character(purrr::map(ids, "pmid")),
                        33,
                        1000000L
                    )
                ) %>%
                mutate(
                    citing_doi = substr(doi, 17, 1000000L)
                ) %>%
                rename(citing_openalex_id = id) %>%
                mutate(
                    citing_openalex_id = substr(citing_openalex_id, 22, 1000000L)
                ) %>%
                rename(citing_pubdate = publication_date) %>%
                rename(citing_title = display_name) %>%
                select(citing_doi, citing_pmid, citing_openalex_id, citing_title, citing_pubdate)

            ## Count the PMIDs found
            n_pmids_found <- trial_cites %>%
                mutate(is_pmid = grepl("^[0-9]+\\.?[0-9]+$", citing_pmid)) %>%
                pull(is_pmid) %>%
                sum()

            ## Get rid of anything that got into the PMID column that
            ## isn't a well-formed PMID
            trial_cites <- trial_cites %>%
                mutate(
                    citing_pmid = ifelse(
                        grepl("^[0-9]+\\.?[0-9]+$", citing_pmid),
                        citing_pmid,
                        NA
                    )
                )

            ## Keep trying to get the animal studies column until
            ## they're all successfully downloaded or until the 10th
            ## try
            trial_cites_with_animals <- NA
            animal_dl_success <- 0
            total_retries <- 10
            n_retry <- 1
            while(animal_dl_success != 1 & n_retry < total_retries) {
                trial_cites_with_animals <- intersection_check(
                    trial_cites,
                    "citing_pmid",
                    animals_query,
                    pmak
                )
                if (n_pmids_found > 0) {
                    animal_dl_success <- trial_cites_with_animals %>%
                        filter(! is.na(citing_pmid) & ! citing_pmid == "") %>%
                        pull(pm_checked) %>%
                        mean()
                } else {
                    animal_dl_success <- 1
                }
                n_retry <- n_retry + 1
            }
            ## Clean up the data frame
            if (is.data.frame(trial_cites_with_animals)) {
                trial_cites <- trial_cites_with_animals %>%
                    select(! pm_checked) %>%
                    rename(citing_is_animal = found_in_pm_query)                
            } else {
                trial_cites$citing_is_animal <- as.character(NA)
            }

            message(
                paste(
                    sum(trial_cites$citing_is_animal, na.rm=TRUE),
                    "animal study(-ies) identified"
                )
            )

            ## Keep trying to get the clinical trial column until
            ## they're all successfully downloaded or until the 10th
            ## try
            trial_cites_with_ct <- NA
            ct_dl_success <- 0
            total_retries <- 10
            n_retry <- 1
            while (ct_dl_success != 1 & n_retry < total_retries) {
                trial_cites_with_ct <- intersection_check(
                    trial_cites,
                    "citing_pmid",
                    clinical_query,
                    pmak
                )
                if (n_pmids_found > 0) {
                    ct_dl_success <- trial_cites_with_ct %>%
                        filter(! is.na(citing_pmid) & ! citing_pmid == "") %>%
                        pull(pm_checked) %>%
                        mean()
                } else {
                    ct_dl_success <- 1
                }
                n_retry <- n_retry + 1
            }
            ## Clean up the data frame
            if (is.data.frame(trial_cites_with_ct)) {
                trial_cites <- trial_cites_with_ct %>%
                    select(! pm_checked) %>%
                    rename(citing_is_clinical = found_in_pm_query)
            } else {
                trial_cites$citing_is_clinical <- as.character(NA)
            }
            
            message(
                paste(
                    sum(trial_cites$citing_is_clinical, na.rm=TRUE),
                    "clinical trial(s) identified"
                )
            )

            ## Now get the Scite data
            sc_off <- 0
            sc_lim <- 100
            n_scite_returned <- NA
            scite_data <- list()
            scite_cites <- tribble(
                ~citing_doi, ~cite_type
            )
            newrow <- NA

            while( is.na(n_scite_returned) | sc_off < n_scite_returned ) {
                scite_response <- get_citations_from_scite(scak, trial_doi, sc_off, sc_lim)

                if (is.na(n_scite_returned)) {
                    n_scite_returned <- scite_response$metadata$totalCitationCount
                }

                if (n_scite_returned > 0) {
                    newrow <- as_tibble(scite_response$citations) %>%
                        rename(citing_doi = source) %>%
                        rename(cite_type = type)

                    scite_cites <- scite_cites %>%
                        bind_rows(newrow)
                }

                sc_off <- sc_off + sc_lim
            }

            ## Collapse in the case of multiple citations in a single paper
            scite_cites <- scite_cites %>%
                distinct(citing_doi, cite_type) %>%
                group_by(citing_doi) %>%
                arrange(cite_type) %>%
                summarize(cite_type = paste(cite_type, collapse = " "))

            if (nrow(scite_cites) > 0) {                
                trial_cites <- trial_cites %>%
                    left_join(scite_cites, by=join_by(citing_doi))
            } else {
                trial_cites$cite_type <- as.character(NA)
            }

            ## Add in the columns of info about the trial being cited
            trial_cites <- trial_cites %>%
                mutate(
                    trial_doi = trial_doi
                ) %>%
                mutate(
                    trial_pmid = trial_pmid
                ) %>%
                mutate(
                    trial_openalex_id = trial_openalex_id
                ) %>%
                mutate(
                    trial_title = trial_title
                ) %>%
                mutate(
                    trial_pubdate = trial_pubdate
                ) %>%
                select(
                    trial_doi,
                    trial_pmid,
                    trial_openalex_id,
                    trial_title,
                    trial_pubdate,
                    citing_doi,
                    citing_pmid,
                    citing_openalex_id,
                    citing_title,
                    citing_pubdate,
                    citing_is_animal,
                    citing_is_clinical,
                    cite_type
                )

            trial_cites %>%
                write_csv(citationsfile, append=TRUE)

            readfiles <- TRUE
            
        } else {
            ## Add the trial to the list of ones that got no cites
            ## within 5 years
            tribble(
                ~trial_doi,
                ~trial_pmid,
                ~trial_title,
                ~trial_pubdate,
                trial_doi,
                trial_pmid,
                trial_title,
                as.Date(trial_pubdate)
            ) %>%
                write_csv(nocitationsfile, append=TRUE)

            readfiles <- TRUE
        }

        ## Print out progress
        prog_denom <- length(unique(trials_with_ids$doi))
        prog_numer <- nrow(distinct(citations, trial_doi)) +
            nrow(distinct(nocitations, trial_doi))
        message(
            paste0(
                now(),
                " ",
                prog_numer,
                " of ",
                prog_denom,
                " done (",
                round(100*prog_numer/prog_denom, digits=2),
                "%)"
            )
        )
        
    } else {
        ## The ID is already processed; no need to re-read the files
        ## as they haven't changed
        readfiles <- FALSE
    }

}
