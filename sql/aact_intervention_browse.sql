COPY (
	SELECT
		bi.id AS browse_intervention_id,
		bi.nct_id,
		bi.mesh_term,
		i.name AS drug_name,
		m.tree_number
	FROM ctgov.browse_interventions bi
		JOIN ctgov.interventions i ON i.nct_id = bi.nct_id
		JOIN ctgov.mesh_terms m ON bi.downcase_mesh_term = m.downcase_mesh_term
	WHERE i.intervention_type ILIKE 'DRUG'
	)
TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t')
	;
