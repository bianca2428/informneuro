library(tidyverse)
library(jsonlite)
library(cthist)

trials <- read_csv("ctg-studies.csv") %>%
    rename(nctid = `NCT Number`) %>%
    slice_head(n=250)

## To use the "latest=TRUE" part, you might need to run the following:
## install_github("bgcarlisle/cthist")

## Download the historical versions
if (! file.exists("hv.csv")) {
    clinicaltrials_gov_download(trials$nctid, "hv.csv", latest=TRUE)
} else {
    message("Whoa there partner! The file hv.csv already exists! (Delete or rename it if you want to re-download)")
}

hv <- read_csv("hv.csv") %>%
    filter(references != "[]") %>%
    filter(! is.na(references)) %>%
    select(nctid, references)

## A function that returns the PMID of the first reference that is of
## the type "RESULT" or NA if there is none
extract_first_results_pmid <- function (jsontxt) {

    first_results <- jsontxt %>%
        fromJSON() %>%
        as_tibble() %>%
        filter(type == "RESULT") %>%
        slice_head()

    if (nrow(first_results) == 1 && "pmid" %in% colnames(first_results)) {
        first_results %>%
            pull(pmid) %>%
            return()
    } else {
        return(NA)
    }

}

## Apply function to data frame to get a new column with the extracted
## PMID
hv$ctg_pmid <- sapply(hv$references, extract_first_results_pmid)

## Keep only rows where PMID's were found and write to disk
if (! file.exists("clinicaltrials_gov_pmids.csv")) {
    hv %>%
        filter(! is.na(ctg_pmid)) %>%
        select(nctid, ctg_pmid) %>%
        mutate(ctg_pmid_link = paste0("https://pubmed.ncbi.nlm.nih.gov/", ctg_pmid, "/")) %>%
        mutate(manually_checked = NA) %>%
        write_csv("clinicaltrials_gov_pmids.csv")    
} else {
    message("Whoa there partner! The file clinicaltrials_gov_pmids.csv already exists! (Delete or rename it if you want to write the PMID's to disk)")
}
## THESE NEED TO BE CHECKED MANUALLY THAT THE PUBLICATION AT THE PMID
## IS THE FINAL RESULTS PUBLICATION FOR THE INDICATED NCT NUMBER
