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
            10
    )
LIMIT
    5;