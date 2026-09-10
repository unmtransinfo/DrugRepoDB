#! /usr/bin/env Rscript
###
##########################################################################
# drugcentral.R - Parse DrugCentral information
# 2016: Developed by Adam Brown.
# 2020-2026: Updated by Jeremy Yang.
# Time to execute: 14s (2026)
##########################################################################
library(readr)
library(data.table)

DATADIR <- paste0(Sys.getenv("HOME"), "/../data/DrugCentral/DrugRepoDb")

t_start <- Sys.time()

## Read
identifier <- read_delim(paste0(DATADIR, "/drugcentral_identifier.tsv"), "\t", col_types = cols(.default = col_character(), parent_match=col_logical()))
setDT(identifier)
setnames(identifier, c("struct_id"), c("DrugCentralID"))
indication <- read_delim(paste0(DATADIR, "/drugcentral_omop_relationship.tsv"), "\t", col_types = cols(.default = col_character()))
setDT(indication)
setnames(indication, c("struct_id"), c("DrugCentralID"))
synonyms <- read_delim(paste0(DATADIR, "/drugcentral_synonyms.tsv"), "\t", col_types = cols(.default = col_character()))
setDT(synonyms)
setnames(synonyms, "id", "DrugCentralID")

# DrugBank IDs
drugcentral <- identifier[id_type=="DRUGBANK_ID", .(DrugCentralID, DrugBankID=identifier)]
drugcentral <- merge(drugcentral, synonyms[preferred_name==1, .(DrugCentralID, name)], by.x="DrugCentralID", by.y="DrugCentralID")

## Indications
indication[, umls_cui := ifelse(umls_cui=='', NA, umls_cui)]
drugcentral$DISEASE_MESH <- sapply(drugcentral$DrugCentralID, function(x) {
    slice <- indication[DrugCentralID == x & relationship_name == 'indication']$concept_name
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- paste(slice, collapse = '|')
    return(out)
})

drugcentral$DISEASE_UMLS <- sapply(drugcentral$DrugCentralID, function(x) {
    slice <- indication[DrugCentralID == x & relationship_name == 'indication']$umls_cui
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- paste(slice, collapse = '|')
    return(out)
})
drugcentral[, DISEASE_UMLS := ifelse(DISEASE_UMLS == '', NA, DISEASE_UMLS)]

## Synonyms
drugcentral$SYNONYM <- sapply(drugcentral$DrugCentralID, function(x) {
    slice <- synonyms[DrugCentralID == x]$name
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- toupper(paste(slice, collapse = '|'))
    return(out)
})

t_elapsed <- (Sys.time()-t_start)
message(sprintf("Elapsed time: %.2f %s", t_elapsed, attr(t_elapsed, "units")))
message("Done: (drugcentral.R)")
