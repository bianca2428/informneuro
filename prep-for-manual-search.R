library(tidyverse)
library(cthist)

                                        # Step 1
                                        # ClinicalTrials.gov scraping

## Read NCT's into memory
trials <- read_csv("2023-12-13-ex-trials.csv")

## Download latest version of each trial
if (! file.exists("2023-12-13-hv.csv")){
    clinicaltrials_gov_download(trials$nctid, "2023-12-13-hv.csv", latest=TRUE)
}

## Read in latest version of trial records
hv <- read_csv("2023-12-13-hv.csv")

## Extract results publications and write to disk
if (! file.exists("2023-12-13-clinicaltrials_gov_found_pmids.csv")) {
    hv %>%
        extract_publications(types="RESULT") %>%
        select(nctid, pmid, citation) %>%
        write_csv("2023-12-13-clinicaltrials_gov_found_pmids.csv")    
}

## In this step, I painstakingly reviewed every single one and wrote it to a new
## CSV file, which I read in here!
ctg_man <- read_csv("2023-12-13-clinicaltrials_gov_found_pmids_manual.csv") %>%
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

ak <- readLines("/home/researchfairy/Academic/2023-12-06-pubmedtk-intersection/api_key.txt")

## Query Pubmed API
trials_pubmed_res <- get_pmids_from_searches(trials_to_check_in_pubmed, "nctid", ak)

if (! file.exists("2023-12-13-pubmed-res.csv")) {
    trials_pubmed_res %>%
        filter(n_results > 0) %>%
        select(nctid, pmids) %>%
        write_csv("2023-12-13-pubmed-res.csv")    
}

pubmed_man <- read_csv("2023-12-13-pubmed-res_manual.csv") %>%
    filter(include) %>%
    select(nctid, pmid)

found_in_ctg_or_pubmed <- ctg_man %>%
    bind_rows(pubmed_man)

trials_to_check_in_gsch_em <- trials %>%
    filter(! nctid %in% found_in_ctg_or_pubmed$nctid)

if (! file.exists("2023-12-13-gsch_em_check.csv")) {
    trials_to_check_in_gsch_em %>%
        write_csv("2023-12-13-gsch_em_check.csv")
}

                                        # Step 3
                                        # Convert PMIDs to DOIs

found_with_mdata <- found_in_ctg_or_pubmed %>%
    get_metadata_from_pmids("pmid", ak) %>%
    select(nctid, doi)

                                        # Step 4
                                        # Combine with Google/Embase manual searches

gsch_em_man <- read_csv("2023-12-13-gsch_check_em_manual.csv") %>%
    filter(include) %>%
    select(nctid, doi)

                                        # Final step
                                        # Put them all together!!

all_found <- found_with_mdata %>%
    bind_rows(gsch_em_man)

if (! file.exists("all-found.csv")) {
    all_found %>%
        write_csv("all-found.csv")
}
