library(tidyverse)
library(ggplot2)
library(cthist) ## https://github.com/bgcarlisle/cthist

extractions <- read_csv("data/neuro-sample.csv") %>%
    rename(nctid = "NCT Number")

## Check that the NCT's are unique
duplicated(extractions$nctid) %>%
    sum()
## (This should be 0)

## Pull out the NCT ID's from the CSV
nctids <- extractions %>%
    select(nctid) %>%
    pull()

clinicaltrials_gov_download(nctids, "historical_versions-neuro.csv")

## Once all the historical versions have been successfully downloaded,
## this will produce historical_versions.csv, which I copied to the data/
## folder.

## Read in the finished, error-free copy of the historical versions data
hv <- read_csv("historical_versions.csv")

## Integrity check
if (sum(extractions$nctid %in% hv$nctid) == nrow(extractions)) {
    message("There is historical data downloaded for every NCT in the extraction data")
} else {
    message("Error: some trials do not have historical data downloaded")
}


if (sum(hv$nctid %in% extractions$nctid) == nrow(hv)) {
    message("There were no extra historical versions downloaded")
} else {
    message("Error: some extra historical versions were downloaded")
}

## Now we will fill in the fields I left blank in the extractions table

## 1st_enrol: for this column, we want the anticipated enrolment recorded
## in the last registration entry prior to trial launch

hv$version_after_launch <- hv$version_date >= hv$study_start_date
hv$version_after_close <- hv$version_date >= hv$primary_completion_date

## Get the enrolment number from the first version of the trial history
## after the trial was launched
enrol_at_launch <- function(nct) {

    versions_after_launch <- hv %>%
        filter(nctid == nct) %>%
        filter(version_after_launch) %>%
        arrange(version_date)

    if (nrow(versions_after_launch) > 0 ) {
        enrol <- versions_after_launch$enrolment[1]
    } else {
        versions_before_launch <- hv %>%
            filter(nctid == nct) %>%
            filter(! version_after_launch) %>%
            arrange(version_date)

        if (nrow(versions_before_launch) > 0) {
            enrol <- versions_before_launch$enrolment[nrow(versions_before_launch)]
        } else {
            enrol <- NA
        }
    }

    return(enrol)
    
}

extractions$`1st_enrol` <- sapply(
    extractions$nctid,
    enrol_at_launch
)

## Get the latest actual enrolment

get_latest_actual_enrol <- function(nct) {

    actual_enrol_versions <- hv %>%
        filter(nctid == nct) %>%
        filter(enrolment_type == "ACTUAL") %>%
        arrange(version_date)

    if ( nrow(actual_enrol_versions) > 0 ) {

        enrol <- actual_enrol_versions$enrolment[nrow(actual_enrol_versions)]
        
    } else {
        enrol <- NA
    }

    return(enrol)
    
}

extractions$latest_actual_enrol <- sapply(
    extractions$nctid,
    get_latest_actual_enrol
)

extractions$enrol_ratio <- extractions$latest_actual_enrol / extractions$`1st_enrol`

## Non-feasible (actual enrol < 85%)
extractions %>%
    filter(enrol_ratio < 0.85) %>%
    arrange(enrol_ratio) %>%
    select(nctid, `1st_enrol`, latest_actual_enrol, enrol_ratio)

## Plot ratios as a histogram
pdf("plots/enrol_ratio.pdf", width=9, height=6)
ggplot(
    aes(
        x = enrol_ratio
    ),
    data = extractions
) +
    geom_histogram(
        binwidth = 0.1
    ) +
    scale_x_continuous(
        breaks = seq(
            from=0,
            to = ceiling(max(extractions$enrol_ratio, na.rm=TRUE)))
    ) +
    geom_vline(
        xintercept = 0.85
    ) +
    labs(
        title = "Ratio of actual to estimated number of patients enrolled with 85% cutoff",
        x = "Ratio of actual to estimated number of patients enrolled",
        y = "Count"
    )
dev.off()
