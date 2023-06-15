#!/usr/bin/env Rscript
###
##########################################################################
# umls_test.R - 
# Queries which should return CUIs:
#	"Atrial fibrillation"
#	"Postpartum depression"
##########################################################################
library(readr)
library(data.table)

if (length(commandArgs(trailingOnly=T))>0) {
  args <- commandArgs(trailingOnly=T)
} else {
  message("ERROR: Syntax: umls_test.R SEARCH_QUERY")
  quit()
}

source('R/umls_query.R')

UMLS_VERSION <- "2023AA"
SEARCH_TYPE <- "normalizedString"

qry <- args[1]
message(sprintf("Search query: \"%s\"", qry))


cuiL <- getCUI(qry, SEARCH_TYPE, UMLS_VERSION, TRUE)

if (length(cuiL) == 0) {
  message(sprintf("No matches for: %s", qry))
} else if (length(cuiL) > 1) {
  message(sprintf("Multiple matches for: %s (%s)", qry, paste(cuiL, collapse="|")))
} else if (cuiL[[1]][1] == 'NO_CONCEPT_MAPPED_TO') {
  message(sprintf("NO_CONCEPT_MAPPED_TO: \"%s\"", qry))
} else {
  cui_this <- cuiL[[1]][1]
  if (!is.na(cui_this)) {
    message(sprintf("Match for: \"%s\" -> %s", qry, cui_this))
  }
}

