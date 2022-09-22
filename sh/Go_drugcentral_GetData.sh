#!/bin/bash
###
# Get DrugCentral data for DrugRepoDB.

set -x

DBHOST="unmtid-dbs.net"
DBNAME="drugcentral"
DBPORT="5433"
DBUSR="drugman"
# Credentials normally in $HOME/.pgpass.

cwd=$(pwd)

###
# identifier.csv
# "id", "identifier", "id_type", "struct_id", "parent_match"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM identifier) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${cwd}/raw/DrugCentral/identifier.tsv

#
###
# synonyms.csv
# "syn_id", "id", "name", "preferred_name", "parent_id", "lname"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM synonyms) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${cwd}/raw/DrugCentral/synonyms.tsv

###
# omop_relationship.csv
# "id", "struct_id", "concept_id", "relationship_name", "concept_name", "umls_cui", "snomed_full_name", "cui_semantic_type", "snomed_conceptid"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM omop_relationship) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${cwd}/raw/DrugCentral/omop_relationship.tsv
