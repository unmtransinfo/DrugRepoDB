library(readr)
library(data.table)

# New webapp datafile:
load('R/repodb/data/repodb.RData')

# Old (2017) webapp data file:
drugs_old <- read_delim("../../Downloads/repodb_full_2017.csv", ",", col_types = cols(.default=col_character()))
setDT(drugs_old)

message(sprintf("New: Drugs (DBIDs): %d; Indications: %d", drugs[, uniqueN(drugbank_id)], drugs[, uniqueN(ind_id)]))
message(sprintf("Old: Drugs (DBIDs): %d; Indications: %d", drugs_old[, uniqueN(drug_id)], drugs_old[, uniqueN(ind_id)]))

message(sprintf("New: Drug trials (NCT_IDs): %d; Terminated: %d; Withdrawn: %d; Suspended: %d; Approved: %d", drugs[, uniqueN(NCT)], drugs[status == "Terminated", uniqueN(NCT)], drugs[status == "Withdrawn", uniqueN(NCT)], drugs[status == "Suspended", uniqueN(NCT)], drugs[status == "Approved", uniqueN(NCT)]))
message(sprintf("Old: Drug trials (NCT_IDs): %d; Terminated: %d; Withdrawn: %d; Suspended: %d; Approved: %d", drugs_old[, uniqueN(NCT)], drugs_old[status == "Terminated", uniqueN(NCT)], drugs_old[status == "Withdrawn", uniqueN(NCT)], drugs_old[status == "Suspended", uniqueN(NCT)], drugs_old[status == "Approved", uniqueN(NCT)]))

message("NCT00454714 present in drugs?...", ("NCT00454714" %in% drugs$NCT))



###
# New clinical trials raw file:
clin <- read_delim("raw/AACT/studies.tsv.gz", "\t", col_types=cols(.default=col_character()))
setDT(clin)

# Old (2017) clinical trials raw file (more rows parsed via readr):
#clin_old <- read_delim("raw/AACT/clinical_study_noclob.txt", "|", quote='"', col_names=T)
clin_old <- read.table('raw/AACT/clinical_study_noclob.txt', sep = '|', quote='"', header = T, fill = T, stringsAsFactors = F)
setDT(clin_old)

clin_cols_old <- tolower(names(clin_old))
message(sprintf("Columns in common:\n\t%s", paste(collapse="\n\t", sort(intersect(names(clin), clin_cols_old)))))
message(sprintf("Columns in Old missing from New:\n\t%s", paste(collapse="\n\t", sort(setdiff(clin_cols_old, names(clin))))))
#message(sprintf("Columns in New missing from Old:\n\t%s", paste(collapse="\n\t", sort(setdiff(names(clin), clin_cols_old)))))

message(sprintf("New: Clinical trials (NCT_IDs): %d", clin[, uniqueN(nct_id)]))
message(sprintf("Old: Clinical trials (NCT_IDs): %d", clin_old[, uniqueN(NCT_ID)]))

# Check for NCT01069861 (Terminated, for Sildenafil)
#print(clin[nct_id == "NCT01069861"])
#print(clin_old[NCT_ID == "NCT01069861"])

# Check for NCT01069861 (Suspended, for Sildenafil)
message("NCT00454714 present in clin?...", ("NCT00454714" %in% clin$nct_id))
print(clin[nct_id == "NCT00454714"])
#print(clin_old[NCT_ID == "NCT00454714"])


###
# New interventions raw file:
intven <- read_delim("raw/AACT/intervention_browse.tsv.gz", "\t", col_types = cols(.default=col_character()))
setDT(intven)

# Old (2017) interventions raw file:
intven_old <- read.table('raw/AACT/intervention_browse.txt', sep='|', header=T, fill=T, stringsAsFactors = F, quote='"')
setDT(intven_old)

###
# New conditions raw file:
cond <- read_delim("raw/AACT/conditions.tsv.gz", "\t")
setDT(cond)

# Old (2017) conditions raw files (NOT IN ZIPFILE):
#cond_old <- fread('raw/AACT/condition_browse.txt', data.table = F)
#cond_old <- rbind(fread('raw/AACT/conditions.txt', data.table = F))

