library(tidyverse)

## Read in the NCT numbers and DOIs from disk
pilot <- read_csv("2024-02-16-bianca-all-pilot.csv") %>%
    mutate(
        doi = tolower(doi)
    )
neuro <- read_csv("2024-02-16-bianca-all-neuro-sample.csv") %>%
    mutate(
        doi = tolower(doi)
    )

## Read in the downloaded citation data
citations <- read_csv("2024-03-05-citations.csv") %>%
    mutate(
        trial_doi = tolower(trial_doi)
    )

## Inspecting the data sets for duplicates
pilot %>%
    group_by(doi) %>%
    count() %>%
    arrange(desc(n))
## There's a few

neuro %>%
    group_by(doi) %>%
    count() %>%
    arrange(desc(n))
## There's also some in the neuro set

## Let's just make sure that these are being folded together correctly
## by the left_join function taking one as an example
pilot %>%
    filter(doi %in% citations$trial_doi) %>%
    left_join(
        citations,
        join_by(doi == trial_doi),
        relationship = "many-to-many"
    ) %>%
    filter(
        doi == "10.1001/jamadermatol.2020.0465"
    ) %>%
    select(nctid, doi, citing_doi) %>%
    print(n=200)

pilot %>%
    filter(doi %in% citations$trial_doi) %>%
    left_join(
        citations,
        join_by(doi == trial_doi),
        relationship = "many-to-many"
    ) %>%
    filter(
        doi == "10.1001/jamadermatol.2020.0465"
    ) %>%
    select(nctid, doi, citing_doi) %>%
    count(citing_doi) %>%
    print(n=200)


pilot %>%
    filter(doi %in% citations$trial_doi) %>%
    left_join(
        citations,
        join_by(doi == trial_doi),
        relationship = "many-to-many"
    ) %>%
    filter(
        doi == "10.1001/jamadermatol.2020.0465" &
        citing_doi == "10.1001/jama.2021.17633"
    ) %>%
    select(nctid, doi, citing_doi)
## So that looks fine

## Write over the pilot data frame with the joined one and write it to
## disk
pilot <- pilot %>%
    filter(doi %in% citations$trial_doi) %>%
    left_join(
        citations,
        join_by(doi == trial_doi),
        relationship = "many-to-many"
    ) %>%
    rename(trial_doi = doi)

pilot %>%
    write_csv("2024-03-06-pilot-citations.csv")

## Do the same for the neuro set
neuro <- neuro %>%
    filter(doi %in% citations$trial_doi) %>%
    left_join(
        citations,
        join_by(doi == trial_doi),
        relationship = "many-to-many"
    ) %>%
    rename(trial_doi = doi)

neuro %>%
    write_csv("2024-03-06-neuro-citations.csv")


## DON'T FORGET

## This only includes the trials that have citations; you have to add
## in the trials without citations if you're making a claim about what
## proportion had no citations at all

## Read in the downloaded no-citation data
nocitations <- read_csv("2024-03-05-no-citations.csv")

## Read in the NCT numbers and DOIs from disk again
pilot <- read_csv("2024-02-16-bianca-all-pilot.csv")
neuro <- read_csv("2024-02-16-bianca-all-neuro-sample.csv")

nocitepilot <- pilot %>%
    filter(doi %in% nocitations$trial_doi) %>%
    left_join(
        nocitations,
        join_by(doi == trial_doi)
    )

nocitepilot %>%
    write_csv("2024-03-06-pilot-nocitations.csv")

nociteneuro <- neuro %>%
    filter(doi %in% nocitations$trial_doi) %>%
    left_join(
        nocitations,
        join_by(doi == trial_doi)
    )

nociteneuro %>%
    write_csv("2024-03-06-neuro-nocitations.csv")
