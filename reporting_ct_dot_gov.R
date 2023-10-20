library(tidyverse)
library(ggplot2)
library(cthist) ## https://github.com/bgcarlisle/cthist

extractions <- read_csv("ctg-studies.csv") %>%
    rename(nctid = "NCT Number") %>%
    head(n=25)

if (! file.exists("historical_versions.csv")) {
    clinicaltrials_gov_download(nctids, "historical_versions.csv")
}

hv <- read_csv("historical_versions.csv")

hv %>%
    select(nctid, version_date, results_posted) %>%
    print(n=100)

hv %>%
    filter(results_posted) %>%
    select(nctid, version_date)

reporting <- hv %>%
    filter(results_posted) %>%
    group_by(nctid) %>%
    slice_head() %>%
    select(nctid, version_date, primary_completion_date) %>%
    mutate(diff = version_date - primary_completion_date)
    
## use `nrow()` and `distinct()` functions to count how many trials
## never post results

## use `nrow()` function to count how many DO post results

## use ggplot to make an histogram of the number of days it takes for
## trials that do post results

## Some example code

hv_ncts <- hv %>%
    select(nctid)

hv

reporting %>%
    write_csv("reporting.csv")
