#!/bin/bash
###
# Get DrugCentral data for DrugRepoDB.

set -x

DBHOST="ec2-54-218-143-107.us-west-2.compute.amazonaws.com"
DBNAME="drugcentral"
DBPORT="5433"
DBUSR="drugman"
# Credentials normally in $HOME/.pgpass.

###
# identifier.csv
# "id", "identifier", "id_type", "struct_id", "parent_match"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM identifier) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >raw/DrugCentral/identifier.tsv

#
###
# synonyms.csv
# "syn_id", "id", "name", "preferred_name", "parent_id", "lname"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM synonyms) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >raw/DrugCentral/synonyms.tsv

###
# omop_relationship.csv
# "id", "struct_id", "concept_id", "relationship_name", "concept_name", "umls_cui", "snomed_full_name", "cui_semantic_type", "snomed_conceptid"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM omop_relationship) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >raw/DrugCentral/omop_relationship.tsv
