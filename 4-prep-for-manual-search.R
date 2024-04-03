library(tidyverse)
library(cthist)

                                        # Step 1
                                        # ClinicalTrials.gov scraping

## Read NCT's into memory
trials <- read_csv("data/neuro-sample.csv")

## Download latest version of each trial
if (! file.exists("data/latest.csv")){
    clinicaltrials_gov_download(trials$nctid, "data/latest.csv", latest=TRUE)
}

## Read in latest version of trial records
hv <- read_csv("data/latest.csv")

## Extract results publications and write to disk
if (! file.exists("data/clinicaltrials_gov_found_pmids.csv")) {
    hv %>%
        extract_publications(types="RESULT") %>%
        select(nctid, pmid, citation) %>%
        write_csv("data/clinicaltrials_gov_found_pmids.csv")
}

## In this step, I painstakingly reviewed every single one and wrote it to a new
## CSV file, which I read in here!
ctg_man <- read_csv("data/clinicaltrials_gov_found_pmids_manual.csv") %>%
    filter(include) %>%
    select(nctid, pmid)

## Remove trials that we don't need to look for anymore
trials_to_check_in_pubmed <- trials %>%
    filter(! nctid %in% ctg_man$nctid)

                                        # Step 2
                                        # Pubmed scraping
## Optional, if necessary:

## library(devtools)
## install_github("bgcarlisle/pubmedtk")

library(pubmedtk)

ak <- readLines("data/pubmed_api_key.txt")

## Query Pubmed API
trials_pubmed_res <- get_pmids_from_searches(trials_to_check_in_pubmed, "nctid", ak)

if (! file.exists("data/pubmed-res.csv")) {
    trials_pubmed_res %>%
        filter(n_results > 0) %>%
        select(nctid, pmids) %>%
        write_csv("data/pubmed-res.csv")    
}

pubmed_man <- read_csv("data/pubmed-res_manual.csv") %>%
    filter(include) %>%
    select(nctid, pmid)

found_in_ctg_or_pubmed <- ctg_man %>%
    bind_rows(pubmed_man)

trials_to_check_in_gsch_em <- trials %>%
    filter(! nctid %in% found_in_ctg_or_pubmed$nctid)

if (! file.exists("data/gsch_em_check.csv")) {
    trials_to_check_in_gsch_em %>%
        write_csv("data/gsch_em_check.csv")
}

                                        # Step 3
                                        # Convert PMIDs to DOIs

found_with_mdata <- found_in_ctg_or_pubmed %>%
    get_metadata_from_pmids("pmid", ak) %>%
    select(nctid, doi)

                                        # Step 4
                                        # Combine with Google/Embase manual searches

gsch_em_man <- read_csv("data/gsch_check_em_manual.csv") %>%
    filter(include) %>%
    select(nctid, doi)

                                        # Final step
                                        # Put them all together!!

all_found <- found_with_mdata %>%
    bind_rows(gsch_em_man)

if (! file.exists("data/all-pubs-found.csv")) {
    all_found %>%
        write_csv("data/all-pubs-found.csv")
}
