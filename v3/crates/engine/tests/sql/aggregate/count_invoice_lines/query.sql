SELECT
    invoiceId, COUNT(*)
FROM
    InvoiceLine
GROUP BY
    invoiceId
LIMIT 10;