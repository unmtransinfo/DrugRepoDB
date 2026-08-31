COPY
	(
	SELECT
		nct_id,
		mesh_term
	FROM ctgov.browse_conditions
	)
TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t') 
	;
