#!/usr/bin/env Rscript
###
##########################################################################
# assemble.R - Prepare data for shiny app
# 2016:  Adam Brown; last update 11/16/16
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT snapshot.
# DrugBank maybe not available, nor needed.
##########################################################################
library(readr)
library(data.table)

## Scripts (order important)
source('R/drugcentral.R')
source('R/clinicaltrials_gov.R')
source('R/umls_query.R')

## Build Indictaion Dictionary
inddict <- data.table(raw=unlist(strsplit(drugcentral$DISEASE_MESH, '\\|')), cui=unlist(strsplit(drugcentral$DISEASE_UMLS, '\\|')), cuname=NA, semType=NA)
inddict <- rbindlist(list(inddict, data.table(raw=unlist(strsplit(clin$DISEASE_MESH,'\\|')), cui=NA, cuname=NA, semType=NA)))
inddict[, cui := ifelse(cui=='NA', NA, cui)]
inddict <- unique(inddict)
for (i in 1:nrow(inddict)) {
    # Get current
    raw <- inddict$raw[i]

    # If missing cui, attempt to fill
    if (is.na(inddict$cui[i])) {
        cuiL <- getCUI(raw, 'normalizedString', '2016AB', F)
        if (length(cuiL) == 1 & cuiL[[1]][1] != 'NO_CONCEPT_MAPPED_TO') inddict$cui[i] <- cuiL[[1]][1]
        # Don't allow multiple/no matches
        else inddict$cui[i] <- NA
    }
    
    # Once filled, check if still NA
    if (is.na(inddict$cui[i])) next
    else {
        inddict$cuname[i] <- getName(inddict$cui[i])
        inddict$semType[i] <- getSemTyp(inddict$cui[i])
    }
}
inddict <- inddict[!is.na(cui) & !is.na(cuname)]
inddict <- unique(inddict)
inddict <- inddict[!duplicated(cuname)]
inddict <- inddict[semType %in% c('Disease or Syndrome', 'Neoplastic Process', 'Pathologic Function', 'Finding', 'Mental or Behavioral Dysfunction',
                                          'Sign or Symptom', 'Injury or Poisoning', 'Congenital Abnormality', 'Acquired Abnormality',
                                          'Cell or Molecular Dysfunction')]

save(inddict, file='raw/indication_dictionary.RData')

## Build dataframe
drug.dt <- data.table(Drug = character(), Indication = character(),
                      drug_name = character(), drug_id = character(),
                      ind_name = character(), ind_id=character(),
                      sem_type = character())
for (i in 1:nrow(drugcentral)) {
    # Drug Handling
    drug <- drugcentral$name[i]
    id <- drugcentral$identifier[i]
    drugcomp <- paste0('<a href="http://www.drugbank.ca/drugs/',id,'" target="_blank">',drug,' (DBID: ', id, ')</a>')
    
    # Indication Handling
    if (is.na(drugcentral$DISEASE_MESH[i])) next
    inds <- unlist(strsplit(drugcentral$DISEASE_MESH[i],'\\|'))
    indcus <- unname(unlist(sapply(inds, function(x) subset(inddict, raw == x)$cui)))
    if (length(indcus) == 0) next
    indcunames <- unname(unlist(sapply(inds, function(x) subset(inddict, raw == x)$cuname)))
    indtypes <- unname(unlist(sapply(inds, function(x) subset(inddict, raw == x)$semType)))
    indcomp <- paste0(indcunames, ' (CUI: ', indcus, ')')
    
    # Expand
    comp_dt <- data.table(Drug = rep(drugcomp, length(indcomp)), Indication = indcomp,
                         drug_name = rep(drug, length(indcomp)), drug_id = rep(id, length(indcomp)),
                         ind_name = indcunames, ind_id = indcus,
                         sem_type = indtypes)
    drug_dt <- rbind(drug_dt, comp_dt)
}

# Add status = Approved, Placeholder for ExLink
drug_dt[, TrialStatus := rep('Approved', nrow(drug_dt))]
drug_dt[, status := rep('Approved', nrow(drug_dt))]
drug_dt[, phase := rep(NA, nrow(drug_dt))]
drug_dt[, DetailedStatus := rep(NA, nrow(drug_dt))]

# Failed drugs from Clinical Trials
failed <- data.table(Drug = character(), Indication = character(),
                     drug_name = character(), drug_id = character(),
                     ind_name = character(), ind_id=character(),
                     sem_type = character())
for (i in 1:nrow(clin)) {
    # Drug Handling
    drugs <- unlist(strsplit(clin$DBNAME[i], '\\|'))
    ids <- unlist(strsplit(clin$identifier[i], '\\|'))
    drugcomp <- paste0('<a href="http://www.drugbank.ca/drugs/', ids, '" target="_blank">', drugs, ' (DBID: ', ids,  ')</a>')
    
    # Indication Handling
    inds <- unlist(strsplit(clin$DISEASE_MESH[i], '\\|'))
    indcus <- unname(unlist(sapply(inds, function(x) inddict[raw == x]$cui)))
    if (length(indcus) == 0) next
    indcunames <- unname(unlist(sapply(inds, function(x) inddict[raw == x]$cuname)))
    indtypes <- unname(unlist(sapply(inds, function(x) inddict[raw == x]$semType)))
    indcomp <- paste0(indcunames, ' (CUI: ', indcus, ')')

    # Expand
    comp_dt_set <- expand.grid(c(1:length(drugcomp)), c(1:length(indcomp)))
    comp_dt <- data.table(Drug = drugcomp[comp_dt_set$Var1], Indication = indcomp[comp_dt_set$Var2],
                         drug_name = drugs[comp_dt_set$Var1], drug_id = ids[comp_dt_set$Var1],
                         ind_name = indcunames[comp_dt_set$Var2], ind_id = indcus[comp_dt_set$Var2],
                         sem_type = indtypes[comp_dt_set$Var2])
    
    # Add status
    comp_dt[, TrialStatus := paste0('<a href="https://clinicaltrials.gov/ct2/show/', clin$NCT_ID[i], '" target="_blank">', clin$OVERALL_STATUS[i], ' (', clin$PHASE[i], ')</a>')]
    comp_dt[, status := clin$OVERALL_STATUS[i]]
    comp_dt[, phase := clin$PHASE[i]]
    comp_dt[, DetailedStatus := clin$WHY_STOPPED[i]]
    
    # Ensure uniqueness
    goodrows <- rep(NA, nrow(comp_dt))
    for (j in 1:nrow(comp_dt)) {
        if (nrow(drug_dt[Drug == comp_dt$Drug[j] & Indication == comp_dt$Indication[j]]) == 0) goodrows[j] <- TRUE
        else goodrows[j] <- FALSE
    }
    
    # Add to dt
    failed <- rbindlist(list(failed, comp_dt[goodrows]))
}

## Combine
drug_dt <- rbindlist(list(drug_dt, failed))
drug_dt <- unique(drug_dt)
drug_dt[, NCT := gsub('<a href="https://clinicaltrials.gov/ct2/show/|" target="_blank">.*|Approved', '', TrialStatus)]
drug_dt[, NCT := ifelse(NCT == '', NA, NCT)]

########
# Save #
########

save(drug_dt, file='R/drugrepodb/data/app.RData')
