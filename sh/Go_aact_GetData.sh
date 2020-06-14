#!/bin/bash
###
# Get AACT data for DrugRepoDB.
# For an account see https://aact.ctti-clinicaltrials.org/connect
# Alternately, download from https://aact.ctti-clinicaltrials.org/pipe_files

set -x

#
DBHOST="aact-db.ctti-clinicaltrials.org"
DBPORT="5432"
DBNAME="aact"
DBUSR="jjyang"

# clinical_study_noclob.txt
# NCT_ID|DOWNLOAD_DATE|DOWNLOAD_DATE_DT|ORG_STUDY_ID|BRIEF_TITLE|OFFICIAL_TITLE|ACRONYM|SOURCE|HAS_DMC|OVERALL_STATUS|START_DATE|COMPLETION_DATE|COMPLETION_DATE_TYPE|PRIMARY_COMPLETION_DATE|PRIMARY_COMPLETION_DATE_TYPE|PHASE|STUDY_TYPE|STUDY_DESIGN|NUMBER_OF_ARMS|NUMBER_OF_GROUPS|ENROLLMENT_TYPE|ENROLLMENT|BIOSPEC_RETENTION|BIOSPEC_DESCR|GENDER|MINIMUM_AGE|MAXIMUM_AGE|HEALTHY_VOLUNTEERS|SAMPLING_METHOD|STUDY_POP|VERIFICATION_DATE|LASTCHANGED_DATE|FIRSTRECEIVED_DATE|IS_SECTION_801|IS_FDA_REGULATED|WHY_STOPPED|HAS_EXPANDED_ACCESS|FIRSTRECEIVED_RESULTS_DATE|URL|TARGET_DURATION|STUDY_RANK|LIMITATIONS_AND_CAVEATS

#
cols="\
s.nct_id, \
s.nlm_download_date_description, \
s.study_first_submitted_date, \
s.results_first_submitted_date, \
s.disposition_first_submitted_date, \
s.last_update_submitted_date, \
s.study_first_submitted_qc_date, \
s.study_first_posted_date, \
s.study_first_posted_date_type, \
s.results_first_submitted_qc_date, \
s.results_first_posted_date, \
s.results_first_posted_date_type, \
s.disposition_first_submitted_qc_date, \
s.disposition_first_posted_date, \
s.disposition_first_posted_date_type, \
s.last_update_submitted_qc_date, \
s.last_update_posted_date, \
s.last_update_posted_date_type, \
s.start_date_type, \
s.start_date, \
s.verification_date, \
s.completion_date_type, \
s.completion_date, \
s.primary_completion_date_type, \
s.primary_completion_date, \
s.target_duration, \
s.study_type, \
s.acronym, \
s.baseline_population, \
s.brief_title, \
s.official_title, \
s.overall_status, \
s.last_known_status, \
s.phase, \
s.enrollment, \
s.enrollment_type, \
s.source, \
s.limitations_and_caveats, \
s.number_of_arms, \
s.number_of_groups, \
s.biospec_retention, \
s.biospec_description, \
s.why_stopped, \
s.has_expanded_access, \
s.expanded_access_type_individual, \
s.expanded_access_type_intermediate, \
s.expanded_access_type_treatment, \
s.has_dmc, \
s.is_fda_regulated_drug, \
s.is_fda_regulated_device, \
s.is_unapproved_device, \
s.is_ppsd, \
s.is_us_export, \
s.ipd_time_frame, \
s.ipd_access_criteria, \
s.ipd_url, \
s.plan_to_share_ipd, \
s.plan_to_share_ipd_description, \
s.created_at, \
s.updated_at"

###
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT $cols, i.id AS intervention_id, i.name AS drug_name FROM studies s JOIN interventions i ON i.nct_id = s.nct_id WHERE s.study_type = 'Interventional' AND i.intervention_type = 'Drug')  TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" |gzip -c >raw/AACT/studies.tsv.gz

###
# intervention_browse.txt
# MESH_INTERVENTION_ID|NCT_ID|MESH_TERM

###
psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT bi.id AS browse_intervention_id, bi.nct_id, bi.mesh_term, i.name AS drug_name FROM browse_interventions bi JOIN interventions i ON i.nct_id = bi.nct_id WHERE i.intervention_type = 'Drug')  TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" |gzip -c >raw/AACT/intervention_browse.tsv.gz

###
# condition_browse.txt ??

# conditions.txt ??
# NCT_ID, CONDITION, ?
