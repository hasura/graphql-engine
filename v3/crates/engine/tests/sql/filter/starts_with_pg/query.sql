SELECT
    *
FROM
    Authors
WHERE
    first_name ~~ 'P%'
LIMIT
    10;