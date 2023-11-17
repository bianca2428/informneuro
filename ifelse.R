## Here's an example script for the mutate and ifelse functions

library(tidyverse)

hv <- read_csv("historical_versions.csv")

hv %>%
    group_by(nctid) %>%
    slice_tail() %>%
    mutate(
        refs = references != "[]"
    ) %>%
    select(nctid, refs) %>%
    filter(! refs)
