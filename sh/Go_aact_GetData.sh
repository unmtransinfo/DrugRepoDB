#!/bin/bash
###
# Get AACT data for DrugRepoDB.
# For an account see https://aact.ctti-clinicaltrials.org/connect
# Alternately, download from https://aact.ctti-clinicaltrials.org/pipe_files

# Database content refreshed daily according to website. 

#
DBHOST="aact-db.ctti-clinicaltrials.org"
DBPORT="5432"
DBNAME="aact"
DBSCHEMA="ctgov"
DBUSR="jjyang"
# DBPW from $HOME/.pgpass

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
###
# clinical_study_noclob.txt
# NCT_ID|DOWNLOAD_DATE|DOWNLOAD_DATE_DT|ORG_STUDY_ID|BRIEF_TITLE|OFFICIAL_TITLE|ACRONYM|SOURCE|HAS_DMC|OVERALL_STATUS|START_DATE|COMPLETION_DATE|COMPLETION_DATE_TYPE|PRIMARY_COMPLETION_DATE|PRIMARY_COMPLETION_DATE_TYPE|PHASE|STUDY_TYPE|STUDY_DESIGN|NUMBER_OF_ARMS|NUMBER_OF_GROUPS|ENROLLMENT_TYPE|ENROLLMENT|BIOSPEC_RETENTION|BIOSPEC_DESCR|GENDER|MINIMUM_AGE|MAXIMUM_AGE|HEALTHY_VOLUNTEERS|SAMPLING_METHOD|STUDY_POP|VERIFICATION_DATE|LASTCHANGED_DATE|FIRSTRECEIVED_DATE|IS_SECTION_801|IS_FDA_REGULATED|WHY_STOPPED|HAS_EXPANDED_ACCESS|FIRSTRECEIVED_RESULTS_DATE|URL|TARGET_DURATION|STUDY_RANK|LIMITATIONS_AND_CAVEATS

#
#

set -x

#
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR \
	-f ${cwd}/sql/aact_studies.sql \
	|gzip -c >${DATADIR}/aact_studies.tsv.gz

###
# intervention_browse.txt
# MESH_INTERVENTION_ID|NCT_ID|MESH_TERM
# But what is MESH_INTERVENTION_ID? Methinks AACT table id.

$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR \
	-f ${cwd}/sql/aact_intervention_browse.sql \
	|gzip -c >${DATADIR}/aact_intervention_browse.tsv.gz
###
# condition_browse.txt ?? (Not in figshare zipfiles.)
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR \
	-f ${cwd}/sql/aact_condition_browse.sql \
	|gzip -c >${DATADIR}/aact_condition_browse.tsv.gz
###
# conditions.txt ?? (Not in figshare zipfiles.)
# NCT_ID, CONDITION, ?
$PSQL -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR \
	-f ${cwd}/sql/aact_conditions.sql \
	|gzip -c >${DATADIR}/aact_conditions.tsv.gz

###
printf "$(date +'%Y-%m-%d-%H:%M:%S')\n" >${DATADIR}/aact_timestamp.txt
