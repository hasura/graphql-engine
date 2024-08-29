SELECT
    *
FROM
    Invoice
WHERE
    invoiceDate > TIMESTAMP '2010-01-01T00:00:00'
LIMIT
    10;