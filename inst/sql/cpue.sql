SELECT
    year,
    survey,
    species_code,
    catchjoin,
    hauljoin,
    stratum,
    numcpue
FROM goa.cpue 
WHERE species_code 
-- insert species
AND survey
-- insert region
AND year
-- insert year
