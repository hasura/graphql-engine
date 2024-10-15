SELECT
    *
FROM
    (
        SELECT
            *
        FROM
            Invoice
        ORDER BY
            invoiceDate DESC
        LIMIT
            5
        OFFSET 
            5
    )
LIMIT
    5
OFFSET 
    3;