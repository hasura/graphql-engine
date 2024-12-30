EXPLAIN -- Can't actually run this because istarts_with doesn't exist in ndc-postgres
SELECT
    *
FROM
    Authors
WHERE
    LOWER(first_name) ~~ 'p%'
LIMIT
    10;