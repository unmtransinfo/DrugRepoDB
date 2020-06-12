#!/bin/bash
###
# Get AACT data for DrugRepoDB.

DBHOST="aact-db.ctti-clinicaltrials.org"
DBPORT="5432"
DBNAME="aact"
DBUSR="jjyang"

# clinical_study_noclob.txt
# NCT_ID|DOWNLOAD_DATE|DOWNLOAD_DATE_DT|ORG_STUDY_ID|BRIEF_TITLE|OFFICIAL_TITLE|ACRONYM|SOURCE|HAS_DMC|OVERALL_STATUS|START_DATE|COMPLETION_DATE|COMPLETION_DATE_TYPE|PRIMARY_COMPLETION_DATE|PRIMARY_COMPLETION_DATE_TYPE|PHASE|STUDY_TYPE|STUDY_DESIGN|NUMBER_OF_ARMS|NUMBER_OF_GROUPS|ENROLLMENT_TYPE|ENROLLMENT|BIOSPEC_RETENTION|BIOSPEC_DESCR|GENDER|MINIMUM_AGE|MAXIMUM_AGE|HEALTHY_VOLUNTEERS|SAMPLING_METHOD|STUDY_POP|VERIFICATION_DATE|LASTCHANGED_DATE|FIRSTRECEIVED_DATE|IS_SECTION_801|IS_FDA_REGULATED|WHY_STOPPED|HAS_EXPANDED_ACCESS|FIRSTRECEIVED_RESULTS_DATE|URL|TARGET_DURATION|STUDY_RANK|LIMITATIONS_AND_CAVEATS

#drug_name, originator, disease_tissue, phase, enrollment_status, last_known_status, nct_id, start_date, primary_completion_date, completion_date, all_countries, oncology_id, study_type, official_title, interventions, interventions_desc, why_stopped, brief_desc, first_posted_date, last_update_posted_date, created_at, updated_at

#nct_id, nlm_download_date_description, study_first_submitted_date, results_first_submitted_date, disposition_first_submitted_date, last_update_submitted_date, study_first_submitted_qc_date, study_first_posted_date, study_first_posted_date_type, results_first_submitted_qc_date, results_first_posted_date, results_first_posted_date_type, disposition_first_submitted_qc_date, disposition_first_posted_date, disposition_first_posted_date_type, last_update_submitted_qc_date, last_update_posted_date, last_update_posted_date_type, start_month_year, start_date_type, start_date, verification_month_year, verification_date, completion_month_year, completion_date_type, completion_date, primary_completion_month_year, primary_completion_date_type, primary_completion_date, target_duration, study_type, acronym, baseline_population, brief_title, official_title, overall_status, last_known_status, phase, enrollment, enrollment_type, source, limitations_and_caveats, number_of_arms, number_of_groups, biospec_retention, why_stopped, has_expanded_access, expanded_access_type_individual, expanded_access_type_intermediate, expanded_access_type_treatment, has_dmc, is_fda_regulated_drug, is_fda_regulated_device, is_unapproved_device, is_ppsd, is_us_export, biospec_retention, biospec_description, ipd_time_frame, ipd_access_criteria, ipd_url, plan_to_share_ipd, plan_to_share_ipd_description, created_at, updated_at

psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT s.nct_id, s.study_type, s.brief_title, s.official_title, s.overall_status, s.last_known_status, s.phase, s.source, s.created_at, s.updated_at, i.id AS intervention_id, i.name AS drug_name FROM studies s JOIN interventions i ON i.nct_id = s.nct_id WHERE s.study_type = 'Interventional' AND i.intervention_type = 'Drug')  TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >raw/AACT/studies.tsv

# intervention_browse.txt
# MESH_INTERVENTION_ID|NCT_ID|MESH_TERM

psql -h $DBHOST -p $DBPORT -d $DBNAME -U $DBUSR -c "COPY (SELECT bi.id AS browse_intervention_id, bi.nct_id, bi.mesh_term, i.name AS drug_name FROM browse_interventions bi JOIN interventions i ON i.nct_id = bi.nct_id WHERE i.intervention_type = 'Drug')  TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')" >raw/AACT/intervention_browse.tsv
