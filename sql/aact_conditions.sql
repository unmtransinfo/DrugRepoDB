COPY (
	SELECT
		nct_id,
		name
	FROM ctgov.conditions
	)
TO STDOUT WITH (FORMAT CSV,HEADER,DELIMITER E'\t') 
;
