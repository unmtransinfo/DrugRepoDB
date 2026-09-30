#!/bin/bash
###
# Get DrugCentral data for DrugRepoDB.

#set -x

#DBHOST="unmtid-dbs.net"
#DBNAME="drugcentral"
#DBPORT="5433"
#DBUSR="drugman"

DBHOST="localhost"
DBNAME="drugcentral"
DBPORT="5432"
DBUSR="drugman"

# Credentials normally in $HOME/.pgpass.

cwd=$(pwd)

DATADIR="$(cd $HOME/../data/DrugCentral/DrugRepoDb; pwd)"

if [ ! -e "$DATADIR" ]; then
	printf "DATADIR not found: ${DATADIR}\n"
	exit
fi
###
if [ "`uname -s`" = "Darwin" ]; then
	PSQL="/Library/PostgreSQL/18/bin/psql"
else
	PSQL="$(which psql)"
fi
printf "PSQL: ${PSQL}\n"
#

set -x

###
# drugcentral_structure.tsv
# "struct_id", "name", "smiles", "inchikey"
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT id, name, smiles, inchikey FROM structures) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_structure.tsv
#
###
# drugcentral_identifier.tsv
# "id", "identifier", "id_type", "struct_id", "parent_match"
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM identifier) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_identifier.tsv
#
###
# drugcentral_synonyms.tsv
# "syn_id", "id", "name", "preferred_name", "parent_id", "lname"
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM synonyms) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_synonyms.tsv
###
# drugcentral_omop_relationship.tsv
# "id", "struct_id", "concept_id", "relationship_name", "concept_name", "umls_cui", "snomed_full_name", "cui_semantic_type", "snomed_conceptid"
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT * FROM omop_relationship) TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >${DATADIR}/drugcentral_omop_relationship.tsv
