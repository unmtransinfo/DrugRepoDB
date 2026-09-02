#! /usr/bin/env Rscript
###
##########################################################################
# drugcentral.R - Parse DrugCentral information
# 2016: Developed by Adam Brown
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT-20200201.
# DrugBank not available, nor needed.
##########################################################################
# 2022: Updated for DrugCentral-20220822 and AACT-20220922.
##########################################################################
library(readr)
library(data.table)

DATADIR <- paste0(Sys.getenv("HOME"), "/../data/DrugCentral/DrugRepoDb")

t_start <- Sys.time()

## Read
identifier <- read_delim(paste0(DATADIR, "/drugcentral_identifier.tsv"), "\t", col_types = cols(.default = col_character(), parent_match=col_logical()))
setDT(identifier)
indication <- read_delim(paste0(DATADIR, "/drugcentral_omop_relationship.tsv"), "\t", col_types = cols(.default = col_character()))
setDT(indication)
synonyms <- read_delim(paste0(DATADIR, "/drugcentral_synonyms.tsv"), "\t", col_types = cols(.default = col_character()))
setDT(synonyms)
# DrugBank not needed for approval status. Keep DrugBank IDs. Use DrugCentral preferred_name.
#dbapproved <- read_delim('raw/DrugBank/drug_links.csv', sep=',',quote='"',header=T,stringsAsFactors = F)

## DrugBank IDs
#drugcentral <- subset(identifier, identifier %in% dbapproved$DrugBank.ID & id_type == 'DRUGBANK_ID', select = c('struct_id', 'identifier'))
#drugcentral$name <- sapply(drugcentral$identifier, function(x) subset(dbapproved, DrugBank.ID == x)$Name)


drugcentral <- identifier[id_type=="DRUGBANK_ID", .(struct_id, DrugBankID=identifier)]
drugcentral <- merge(drugcentral, synonyms[preferred_name==1, .(id, name)], by.x="struct_id", by.y="id")

## Indications
indication[, umls_cui := ifelse(umls_cui=='', NA, umls_cui)]
drugcentral$DISEASE_MESH <- sapply(drugcentral$struct_id, function(x) {
    slice <- indication[struct_id == x & relationship_name == 'indication']$concept_name
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- paste(slice, collapse = '|')
    return(out)
})

drugcentral$DISEASE_UMLS <- sapply(drugcentral$struct_id, function(x) {
    slice <- indication[struct_id == x & relationship_name == 'indication']$umls_cui
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- paste(slice, collapse = '|')
    return(out)
})
drugcentral[, DISEASE_UMLS := ifelse(DISEASE_UMLS == '', NA, DISEASE_UMLS)]

## Synonyms
drugcentral$SYNONYM <- sapply(drugcentral$struct_id, function(x) {
    slice <- synonyms[id == x]$name
    if (length(slice) == 0) out <- NA
    else if (length(slice) == 1) out <- slice
    else out <- toupper(paste(slice, collapse = '|'))
    return(out)
})

t_elapsed <- (Sys.time()-t_start)
message(sprintf("Elapsed time: %.2f %s", t_elapsed, attr(t_elapsed, "units")))
message("Done: (drugcentral.R)")
