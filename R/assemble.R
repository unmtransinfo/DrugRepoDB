#!/usr/bin/env Rscript
###
##########################################################################
# assemble.R - Prepare data for shiny app
# 2016:  Adam Brown; last update 11/16/16
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT-20200201.
# DrugBank maybe not available, nor needed.
##########################################################################
library(readr)
library(data.table)

## Scripts (order-dependent)
source('R/drugcentral.R')
source('R/clinicaltrials_gov.R')
source('R/umls_query.R')

UMLS_VERSION <- "2020AA"  # In 2016 version was "2016AB".

t0 <- proc.time()

## Build Indication Dictionary
inddict <- data.table(raw=unlist(strsplit(drugcentral$DISEASE_MESH, '\\|')), cui=unlist(strsplit(drugcentral$DISEASE_UMLS, '\\|')), cuname=NA, semType=NA)
inddict <- rbindlist(list(inddict, data.table(raw=unlist(strsplit(clin$DISEASE_MESH,'\\|')), cui=as.character(NA), cuname=as.character(NA), semType=as.character(NA))))
inddict[, cui := ifelse(cui=='NA', NA, cui)]
inddict <- inddict[!is.na(raw)]
inddict <- unique(inddict)
for (i in 1:nrow(inddict)) {
    # Get current
    raw <- inddict$raw[i]
    message(sprintf("%d/%d: %s", i, nrow(inddict), raw))
    # If missing cui, attempt to fill
    if (is.na(inddict$cui[i])) {
        cuiL <- getCUI(raw, 'normalizedString', UMLS_VERSION, F)
        # Don't allow multiple/no matches
        if (length(cuiL) == 0) {
          message(sprintf("No matches for: %s", raw))
          inddict[i, cui := NA]
          next
        }
        else if (length(cuiL) > 1) {
          message(sprintf("Multiple matches for: %s (%s)", raw, paste(cuiL, collapse="|")))
          inddict[i, cui := NA]
          next
        }
        else if (cuiL[[1]][1] == 'NO_CONCEPT_MAPPED_TO') {
          message(sprintf("NO_CONCEPT_MAPPED_TO: \"%s\"", raw))
          inddict[i, cui := NA]
          next
        }
        else {
          cui_this <- cuiL[[1]][1]
          # Confirm not NA
          if (is.na(cui_this)) next
          message(sprintf("Match for: \"%s\" -> %s", raw, cui_this))
          inddict[i, cui := cui_this]
          next
        }
    }
    cuname_this <- getName(inddict[i]$cui)
    semtyp_this <- getSemTyp(inddict[i]$cui)
    message(sprintf("cui=%s; cuname=%s; semtyp=%s", inddict[i, cui], cuname_this, semtyp_this))
    inddict[i, cuname := cuname_this]
    inddict[i, semType := semtyp_this]
}
inddict <- inddict[!is.na(cui) & !is.na(cuname)]
inddict <- unique(inddict)
inddict <- inddict[!duplicated(cuname)]
semType_counts <- inddict[, .(.N), by=semType][order(-N)]
message(sprintf("%2d. %4d %s\n", 1:12, semType_counts[1:12, N], semType_counts[1:12, semType]))
inddict <- inddict[semType %in% c('Disease or Syndrome', 'Neoplastic Process', 'Pathologic Function', 'Finding', 'Mental or Behavioral Dysfunction',
                                          'Sign or Symptom', 'Injury or Poisoning', 'Congenital Abnormality', 'Acquired Abnormality',
                                          'Cell or Molecular Dysfunction')]
# 'Cell or Molecular Dysfunction' still a thing? Apparently no.
save(inddict, file='raw/indication_dictionary.RData')

#load('raw/indication_dictionary.RData') #DEBUG
#stop("DEBUG")

## Build dataframe
drugs <- data.table(Drug = character(), Indication = character(),
                      drug_name = character(), drugbank_id = character(),
                      ind_name = character(), ind_id=character(),
                      sem_type = character())
for (i in 1:nrow(drugcentral)) {
    # Drug Handling
    drugname <- drugcentral$name[i]
    dbid <- drugcentral$DrugBankID[i]
    drugcomp <- sprintf('<a href="http://www.drugbank.ca/drugs/%s" target="_blank">%s (DBID: %s)</a>', dbid, dbid, drugname)
    
    # Indication Handling
    if (is.na(drugcentral$DISEASE_MESH[i])) {
      next
    }
    inds <- unlist(strsplit(drugcentral$DISEASE_MESH[i],'\\|'))
    indcus <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), cui] )))
    if (length(indcus) == 0) {
      next
    }
    indcunames <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), cuname])))
    indtypes <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), semType])))
    indcomp <- paste0(indcunames, ' (CUI: ', indcus, ')')

    # Expand
    comp_dt <- data.table(Drug = rep(drugcomp, length(indcomp)), Indication = indcomp,
                         drug_name = rep(drugname, length(indcomp)), drugbank_id = rep(dbid, length(indcomp)),
                         ind_name = indcunames, ind_id = indcus,
                         sem_type = indtypes)
    drugs <- rbind(drugs, comp_dt)
}

# Add status = Approved, Placeholder for ExLink
drugs[, TrialStatus := 'Approved']
drugs[, status := 'Approved']
drugs[, phase := NA]
drugs[, DetailedStatus := NA]

# Failed drugs from Clinical Trials
failed <- data.table(Drug = character(), Indication = character(),
                     drug_name = character(), drugbank_id = character(),
                     ind_name = character(), ind_id=character(),
                     sem_type = character(),
                     TrialStatus = character(),
                     status = character(),
                     phase = character(),
                     DetailedStatus = character())
for (i in 1:nrow(clin)) {
    if (clin$nct_id[i] == "NCT00454714") stop("DEBUG...")
  
    # Drug Handling
    drugnames <- unlist(strsplit(clin$DCNAME[i], '\\|'))
    dbids <- unlist(strsplit(clin$DrugBankIDs[i], '\\|'))
    drugcomp <- sprintf('<a href="http://www.drugbank.ca/drugs/%s" target="_blank">%s (DBID: %s)</a>', dbids, drugnames, dbids)

    # Indication Handling
    inds <- unlist(strsplit(clin$DISEASE_MESH[i], '\\|'))
    indcus <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), cui])))
    #PROBLEM! NO CUI FOR: "Coronary Vasospasm" https://meshb.nlm.nih.gov/record/ui?ui=D003329
    
    if (length(indcus) == 0) {
      next
    }
    indcunames <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), cuname])))
    indtypes <- unname(unlist(sapply(inds, function(x) inddict[tolower(raw) == tolower(x), semType])))
    indcomp <- paste0(indcunames, ' (CUI: ', indcus, ')')

    # Expand
    comp_dt_set <- expand.grid(c(1:length(drugcomp)), c(1:length(indcomp)))
    comp_dt <- data.table(Drug = drugcomp[comp_dt_set$Var1], Indication = indcomp[comp_dt_set$Var2],
                         drug_name = drugnames[comp_dt_set$Var1], drugbank_id = dbids[comp_dt_set$Var1],
                         ind_name = indcunames[comp_dt_set$Var2], ind_id = indcus[comp_dt_set$Var2],
                         sem_type = indtypes[comp_dt_set$Var2])
    
    # Add status
    comp_dt[, TrialStatus := sprintf('<a href="https://clinicaltrials.gov/ct2/show/%s" target="_blank">%s (%s)</a>', clin$nct_id[i], clin$overall_status[i], clin$phase[i])]
    comp_dt[, status := clin$overall_status[i]]
    comp_dt[, phase := clin$phase[i]]
    comp_dt[, DetailedStatus := clin$why_stopped[i]]

    # Ensure uniqueness
    goodrows <- rep(NA, nrow(comp_dt))
    for (j in 1:nrow(comp_dt)) {
        if (nrow(drugs[Drug == comp_dt$Drug[j] & Indication == comp_dt$Indication[j]]) == 0) goodrows[j] <- TRUE
        else goodrows[j] <- FALSE
    }
    
    # Add to dt
    failed <- rbindlist(list(failed, comp_dt[goodrows]))
}

## Combine
drugs <- rbindlist(list(drugs, failed))
drugs <- unique(drugs)
drugs[, NCT := gsub('<a href="https://clinicaltrials.gov/ct2/show/|" target="_blank">.*|Approved', '', TrialStatus)]
drugs[, NCT := ifelse(NCT == '', NA, NCT)]

########
# Save #
########

save(drugs, file='R/repodb/data/repodb.RData')
#
message(sprintf("%s, elapsed: %.1fs", Sys.time(), (proc.time()-t0)[3]))
