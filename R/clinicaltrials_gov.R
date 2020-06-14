##########################################################################
# clinicaltrials_gov.R - Parse clinical trials information
# 2016: Adam Brown; last Update: 11/16/16
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT snapshot.
# DrugBank maybe not available, nor needed.
##########################################################################
library(readr)
library(data.table)

## Read
#clin <- read.table('raw/AACT/clinical_study_noclob.txt', sep = '|', quote='"', header = T, fill = T, stringsAsFactors = F)
#int <- read.table('raw/AACT/intervention_browse.txt', sep='|', header=T, fill=T, stringsAsFactors = F, quote='"')
#cond <- fread('raw/AACT/condition_browse.txt', data.table = F)
#cond <- rbind(fread('raw/AACT/conditions.txt', data.table = F))

clin <- read_delim("raw/AACT/studies.tsv.gz", "\t", col_types=cols(.default=col_character()))
setDT(clin)
for (tag in colnames(clin)) {
  if (grepl("_(date|at)$", tag)) {
    message(sprintf("Converting to type Date column: %s", tag))
    clin[[tag]] <- as.Date(clin[[tag]])
  }
}

intven <- read_delim("raw/AACT/intervention_browse.tsv.gz", "\t")
setDT(intven)

cond <- read_delim("raw/AACT/conditions.tsv.gz", "\t")
setDT(cond)

## Pull good rows
# NCTID consistent
clin <- clin[grepl("^NCT", nct_id)]
# Phase annotated
clin <- clin[phase %in% c("Phase 0", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3")]
# Failed Only
clin <- clin[overall_status %in% c("Suspended", "Terminated", "Withdrawn")]
# Select useful columns
clin <- clin[, .(nct_id, phase, overall_status, why_stopped)]

## Add interventions
clin$drug_mesh <- sapply(clin$nct_id, function(x) {
    slice <- intven[nct_id == x]$mesh_term
    if (length(slice) == 0) out <- NA
    else {
        slice <- gsub(' drug combination|, ', '', slice)
        out <- toupper(paste(sort(unique(slice)), collapse = '|'))
    }
    return(out)
})
clin <- clin[!is.na(drug_mesh)]

clin$identifier <- sapply(clin$drug_mesh, function(x) {
    mesh <- unlist(strsplit(x, '\\|'))
    greplist <- rep(NA, length(mesh))
    for (i in 1:length(mesh)) {
        greplist[i] <- paste0('^', mesh[i], '$', '|\\|', mesh[i], '$|^', mesh[i], '\\||\\|', mesh[i], '\\|')
    }
    grepcall <- paste(greplist, collapse='|')
    row <- grep(grepcall, drugcentral$SYNONYM)
    if (length(row) == 0) out <- NA
    else out <- paste(unique(drugcentral[row, identifier]), collapse = '|')
})

clin <- clin[!is.na(identifier)]
#clin$DBNAME <- sapply(clin$identifier, function(x) paste(subset(dbapproved, DrugBank.ID %in% unlist(strsplit(x, '\\|')))$Name, collapse='|'))
clin$DCNAME <- sapply(clin$identifier, function(x) paste(drugcentral[identifier %in% unlist(strsplit(x, '\\|'))]$name, collapse='|'))

## Add conditions
clin$DISEASE_MESH <- sapply(clin$nct_id, function(x) {
    slice <- cond[nct_id == x]$name
    out <- paste(slice, collapse = '|')
    return(out)
})
