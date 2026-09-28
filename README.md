# A Standard Database for Drug Repositioning

___NOTE: Forked from [repoDB](https://github.com/adam-sam-brown/repoDB) 
in cooperation with original authors, for development, maintenance, and updates by UNM.___

## Authors

  * Adam S Brow, Harvard Medical School.
  * Chirag J Patel, Harvard Medical School.
  * Jeremy J Yang, UNM School of Medicine.

## Description

Drug repositioning, the process of discovering, validating, and marketing previously approved drugs for new indications, is of growing interest to academia and industry due to reduced time and costs associated with repositioned drugs. Computational methods for repositioning are appealing because they putatively nominate the most promising candidate drugs for a given indication. Comparing the wide array of computational repositioning methods, however, is a challenge due to inconsistencies in method validation in the field. Furthermore, a common simplifying assumption, that all novel predictions are false, is intellectually unsatisfying and hinders reproducibility. We address this assumption by providing a gold standard database, repoDB, that consists of both true positives (approved drugs), and true negatives (failed drugs). We have made the full database and all code used to prepare it publicly available, and have developed a web application that allows users to browse subsets of the data (http://apps.chiragjpgroup.org/repoDB/).

## Publications

* [Adam S. Brown & Chirag J. Patel, A standard database for drug repositioning, Scientific Data volume 4, Article number: 170029 (2017)](https://www.nature.com/articles/sdata201729)
* [Adam S Brown, Chirag J Patel, A review of validation strategies for * computational drug repositioning, Briefings in Bioinformatics, Volume 19, Issue * 1, January 2018, Pages 174–177](https://doi.org/10.1093/bib/bbw110).
* [repoDB: Antidote to an Unsatisfying Assumption, March 26, 2017](https://dbmi.hms.harvard.edu/news/repodb-antidote-unsatisfying-assumption).

## Release Notes 

Summary of 2022:

* New version of DrugCentral, August 22, 2022.
* New version of AACT, accessed September 2022.
* New version of UMLS (2022AA, previously 2020AA).

Summary of 2020 update:

* New version of DrugCentral, May 16, 2020.
* New version of AACT, accessed June 2020.
* New version of UMLS (2020AA, previously 2016AB).
* Created new bash scripts to automate data extraction from DrugCentral and AACT, using psql/SQL.
* Revised R code to rely on DrugCentral instead of DrugBank for approval status, but retaining DrugBank IDs.
* Deployed at <https://unmtid-shinyapps.net/repodb/>.

Summary of 2026 update:

* New version of DrugCentral, September 02, 2026.
* New version of AACT, accessed September 2026.
* New version of UMLS (2026AA, previously 2020AA).
* DrugCentralIDs as primary identifier, throughout UI and downloads, DrugBankIDs included as alternate IDs.

## Workflow

1. [Go\_drugcentral\_GetData.sh](sh/Go_drugcentral_GetData.sh)
1. [Go\_aact\_GetData.sh](sh/Go_aact_GetData.sh)
1. Execute in this order in same R environment:

```
source("R/drugcentral.R")
source("R/clinicaltrials_gov.R")
source("R/umls_query.R")
source("R/assemble.R")
```

## Dependencies

* R packages: `data.table`, `readr`, `stringr`, `yaml`, `httr`, `xml2`, `shiny`, `DT`, `plotly`
