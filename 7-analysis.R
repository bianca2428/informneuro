library(tidyverse)
library(lubridate)

## Read in the pilot data from disk
citations <- read_csv("2024-03-06-pilot-citations.csv")
nocitations <- read_csv("2024-03-06-pilot-nocitations.csv")

citations %>%
    filter(is.na(trial_pubdate)) %>%
    select(nctid, trial_doi, citing_doi)

citations %>%
    filter(is.na(citing_pubdate)) %>%
    select(nctid, trial_doi, citing_doi)

## Add columns for whether the citation happened within n years
citations <- citations %>%
    mutate(
        cited_year1 = citing_pubdate < trial_pubdate + years(1)
    ) %>%
    mutate(
        cited_year2 = citing_pubdate < trial_pubdate + years(2)
    ) %>%
    mutate(
        cited_year3 = citing_pubdate < trial_pubdate + years(3)
    ) %>%
    mutate(
        cited_year4 = citing_pubdate < trial_pubdate + years(4)
    ) %>%
    mutate(
        cited_year5 = TRUE
    )

## Look at citations within 1 year
cite_year1 <- citations %>%
    filter(cited_year1) %>%
    count(nctid)

cite_year2 <- citations %>%
    filter(cited_year2) %>%
    count(nctid)

years <- cite_year1 %>%
    rename(year1 = n) %>%
    left_join(cite_year2) %>%
    rename(year2 = n)

## Don't forget to add the nocites

## Here's the main thing
meaningful <- citations %>%
    filter(cited_year2) %>%
    filter(citing_is_animal | citing_is_clinical) %>%
    filter(grepl("supporting", cite_type) | grepl("contradicting", cite_type)) %>%
    group_by(nctid) %>%
    slice_head() %>%
    select(nctid, trial_doi, trial_pmid, trial_openalex_id, trial_title, trial_pubdate)

## Grouped barplot in ggplot2

## Split by academic/industry sponsor

## This is pseudo code
whereeverthedataisnow <- whereeverthedataisnow %>%
    select(ncid, sponsor, masking, ...)

meaningful <- meaningful %>%
    left_join(
        whereeverthedataisnow,
        join_by(nctid, `NCT Number`(?????))
    )

## Check: do you have the same number of rows as before??

## Now you have meaningful citations with the extra covariates

## Don't forget to also do this to the ones that aren't meaningful AND
## the ones that aren't cited at all

## numerator and denominator for both academic and industry
prop.test(
     c(20, 50),
     c(100, 150)
 )

## Variables to look at
## Sponsor, trial design, condition

ctinfo <- read_csv("neuro-sample.csv")
