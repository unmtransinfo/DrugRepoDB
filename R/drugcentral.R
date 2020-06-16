#! /usr/bin/env Rscript
###
##########################################################################
# drugcentral.R - Parse DrugCentral information
# 2016: Developed by Adam Brown; last update 11/16/16.
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT snapshot.
# DrugBank maybe not available, nor needed.
##########################################################################
library(readr)
library(data.table)

## Read
identifier <- read_delim("raw/DrugCentral/identifier.tsv", "\t", col_types = cols(.default = col_character(), parent_match=col_logical()))
setDT(identifier)
indication <- read_delim("raw/DrugCentral/omop_relationship.tsv", "\t", col_types = cols(.default = col_character()))
setDT(indication)
synonyms <- read_delim("raw/DrugCentral/synonyms.tsv", "\t", col_types = cols(.default = col_character()))
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

message("Done: (drugcentral.R)")