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

DATADIR="$(cd $HOME/../data/DrugCentral/DrugRepoDB; pwd)"

if [ ! -e "$DATADIR" ]; then
	printf "DATADIR not found: ${DATADIR}\n"
	exit
fi

###
# identifier.csv
# "id", "identifier", "id_type", "struct_id", "parent_match"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM identifier) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_identifier.tsv
#
###
# synonyms.csv
# "syn_id", "id", "name", "preferred_name", "parent_id", "lname"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM synonyms) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_synonyms.tsv
###
# omop_relationship.csv
# "id", "struct_id", "concept_id", "relationship_name", "concept_name", "umls_cui", "snomed_full_name", "cui_semantic_type", "snomed_conceptid"
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM omop_relationship) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_omop_relationship.tsv
