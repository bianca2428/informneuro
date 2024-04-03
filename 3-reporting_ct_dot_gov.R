library(tidyverse)
library(ggplot2)
library(cthist) ## https://github.com/bgcarlisle/cthist
library(jsonlite)

extractions <- read_csv("data/neuro-sample.csv") %>%
    rename(nctid = "NCT Number")

if (! file.exists("historical_versions.csv")) {
    clinicaltrials_gov_download(nctids, "data/historical_versions-neuro.csv")
}

hv <- read_csv("data/historical_versions-neuro.csv")

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

reporting %>%
    write_csv("data/reporting.csv")
