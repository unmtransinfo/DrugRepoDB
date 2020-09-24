# A Standard Database for Drug Repositioning

___NOTE: The purpose of this fork (from
<https://github.com/adam-sam-brown/repoDB>) is to update this
resource with the latest versions of DrugCentral, AACT and UMLS.___

## AUTHORS
Adam S Brown (adam-sam-brown) [1], Chirag J Patel (chiragjp) [1]

## AFFILIATIONS
[1] Department of Biomedical Informatics, Harvard Medical School, 10 Shattuck St. Boston, MA 02115.
Corresponding Author: Chirag Patel (e-mail: Chirag_Patel@hms.harvard.edu, tel: (617) 432 1195)

## ABSTRACT
Drug repositioning, the process of discovering, validating, and marketing previously approved drugs for new indications, is of growing interest to academia and industry due to reduced time and costs associated with repositioned drugs. Computational methods for repositioning are appealing because they putatively nominate the most promising candidate drugs for a given indication. Comparing the wide array of computational repositioning methods, however, is a challenge due to inconsistencies in method validation in the field. Furthermore, a common simplifying assumption, that all novel predictions are false, is intellectually unsatisfying and hinders reproducibility. We address this assumption by providing a gold standard database, repoDB, that consists of both true positives (approved drugs), and true negatives (failed drugs). We have made the full database and all code used to prepare it publicly available, and have developed a web application that allows users to browse subsets of the data (http://apps.chiragjpgroup.org/repoDB/).

## PUBLICATIONS

* [Adam S. Brown & Chirag J. Patel, A standard database for drug repositioning, Scientific Data volume 4, Article number: 170029 (2017)](https://www.nature.com/articles/sdata201729)
* [Adam S Brown, Chirag J Patel, A review of validation strategies for * computational drug repositioning, Briefings in Bioinformatics, Volume 19, Issue * 1, January 2018, Pages 174–177](https://doi.org/10.1093/bib/bbw110).
* [repoDB: Antidote to an Unsatisfying Assumption, March 26, 2017](https://dbmi.hms.harvard.edu/news/repodb-antidote-unsatisfying-assumption).

## UPDATE 

Summary of 2020 update by Jeremy Yang of UNM.

* New version of DrugCentral, May 16, 2020.
* New version of UMLS (2020AA).
* New version of CTTI AACT (live instance accessed June 2020).
* Created new bash scripts to automate data extraction from DrugCentral and AACT, using psql/SQL.
* Revised R code to rely on DrugCentral instead of DrugBank for approval status, but retaining DrugBank IDs.
* Deployed provisionally at <http://unmtid-shinyapps.net/repodb/> for testing.

## WORKFLOW

1. [drugcentral.R](R/drugcentral.R)
1. [clinicaltrials_gov.R](R/clinicaltrials_gov.R)
1. [umls_query.R](R/umls_query.R)
1. [assemble.R](R/assemble.R)

