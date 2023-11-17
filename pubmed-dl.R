library(tidyverse)
library(jsonlite)
library(xml2)
library(httr)

apikey <- readLines("api_key.txt")

pmids <- read_csv("pmids.csv")

output_filename <- Sys.time() %>%
    str_replace_all(":", "-") %>%
    str_replace_all(" ", "_") %>%
    paste0("-pubmed-metadata.csv")

tribble(
    ~pmid, ~doi, ~languages_pubmed, ~pubtypes_pubmed, ~authors_pubmed
) %>%
    write_csv(output_filename)

download_pubmed_metadata <- function (pmid, api_key) {

    out <- tryCatch({

        pubmed_search <- list(
            api_key = apikey,
            db = "pubmed",
            term = "NCT03141177",
            retmode="xml"
        )

        res <- POST(
            "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
            body=pubmed_search,
            encode="form"
        )
        
        result <- read_xml(res)

        closeAllConnections()

        numres <- xml_find_all(
            result,
            "/eSearchResult/Count"
        ) %>%
            xml_text() %>%
            as.numeric()
        
        pmids <- xml_find_all(
            result,
            "/eSearchResult/IdList/Id"
        ) %>%
            xml_text()

        return (tribble(
            ~doi, ~languages,        ~pubtypes,        ~authors,
            doi,  toJSON(languages), toJSON(pubtypes), toJSON(authors)
        ))
        
    },
    error=function(cond) {
        message(
            paste(
                "Error:",
                cond
            )
        )

        return(NA)
    },
    warning=function(cond) {
        message(
            paste(
                "Warning:",
                cond
            )
        )

        return(NA)
    },
    finally={
    })

    return(out)
    
}

for (pmid in pmids$pmid) {
    
    meta <- download_pubmed_metadata(pmid)

    tribble(
        ~pmid, ~doi,     ~languages_pubmed, ~pubtypes_pubmed, ~authors_pubmed,
        pmid,  meta$doi, meta$languages,    meta$pubtypes,    meta$authors
    ) %>%
        write_csv(output_filename, append=TRUE)
}
