##########################################################################
# clinicaltrials_gov.R - Parse clinical trials information
# 2016: Developed by Adam Brown
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT-20200201.
# DrugBank not available, nor needed.
##########################################################################
library(readr)
library(data.table)

# drugcentral.R must be run before this code.

DATADIR <- paste0(Sys.getenv("HOME"), "/../data/DrugCentral/DrugRepoDb")

t_start <- Sys.time()

## Read
#clin <- read.table('raw/AACT/clinical_study_noclob.txt', sep = '|', quote='"', header = T, fill = T, stringsAsFactors = F)
#int <- read.table('raw/AACT/intervention_browse.txt', sep='|', header=T, fill=T, stringsAsFactors = F, quote='"')
#cond <- fread('raw/AACT/condition_browse.txt', data.table = F)
#cond <- rbind(fread('raw/AACT/conditions.txt', data.table = F))

clin <- read_delim(paste0(DATADIR, "/aact_studies.tsv.gz"), "\t", col_types=cols(.default=col_character()))
setDT(clin)
for (tag in colnames(clin)) {
  if (grepl("_(date|at)$", tag)) {
    message(sprintf("Column to Date: %s", tag))
    clin[[tag]] <- as.Date(clin[[tag]])
  }
}

message("NCT00454714 in dataset?...", ("NCT00454714" %in% clin$nct_id)) # Check for NCT00454714 (Suspended, for Sildenafil)


intven <- read_delim(paste0(DATADIR, "/aact_intervention_browse.tsv.gz"), "\t", col_types = cols(.default=col_character()))
setDT(intven)

cond <- read_delim(paste0(DATADIR, "/aact_conditions.tsv.gz"), "\t")
setDT(cond)

## Pull good rows
# NCTID consistent
clin <- clin[grepl("^NCT", nct_id)]

# Phase annotated
table(clin$phase)

# 2020: "Phase 0" nonexistent. "Early Phase 1" may be new name.
# 2026: Terms are now UPPERCASE.
clin <- clin[phase %in% c("EARLY_PHASE1", "PHASE1", "PHASE1/PHASE2", "PHASE2", "PHASE2/PHASE3", "PHASE3", "PHASE4")]

table(clin$overall_status)

# Failed Only
clin <- clin[overall_status %in% c("SUSPENDED", "TERMINATED", "WITHDRAWN")]

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

message("NCT00454714 in dataset?...", ("NCT00454714" %in% clin$nct_id)) # Check for NCT00454714 (Suspended, for Sildenafil)


clin$DrugBankIDs <- sapply(clin$drug_mesh, function(x) {
    mesh <- unlist(strsplit(x, '\\|'))
    greplist <- rep(NA, length(mesh))
    for (i in 1:length(mesh)) {
        greplist[i] <- paste0('^', mesh[i], '$', '|\\|', mesh[i], '$|^', mesh[i], '\\||\\|', mesh[i], '\\|')
    }
    grepcall <- paste(greplist, collapse='|')
    row <- grep(grepcall, drugcentral$SYNONYM)
    if (length(row) == 0) out <- NA
    else out <- paste(unique(drugcentral[row, DrugBankID]), collapse = '|')
})
clin <- clin[!is.na(DrugBankIDs)]

message("NCT00454714 in dataset?...", ("NCT00454714" %in% clin$nct_id)) # Check for NCT00454714 (Suspended, for Sildenafil)


clin$DCNAME <- sapply(clin$DrugBankIDs, function(x) paste(drugcentral[DrugBankID %in% unlist(strsplit(x, '\\|')), first(name)], collapse='|'))
# Delimited name count must match delimited DrugBankId count? Check?

## Add conditions
clin$DISEASE_MESH <- sapply(clin$nct_id, function(x) {
    slice <- cond[nct_id == x]$name
    out <- paste(slice, collapse = '|')
    return(out)
})

#message("NCT01069861 in dataset?...", ("NCT01069861" %in% clin$nct_id)) # Check for NCT01069861 (Terminated, for Sildenafil)
#print(clin[nct_id == "NCT01069861"])

message("NCT00454714 in dataset?...", ("NCT00454714" %in% clin$nct_id)) # Check for NCT00454714 (Suspended, for Sildenafil)


t_elapsed <- (Sys.time()-t_start)
message(sprintf("Elapsed time: %.2f %s", t_elapsed, attr(t_elapsed, "units")))
message("Done: (clinicaltrials_gov.R)")
