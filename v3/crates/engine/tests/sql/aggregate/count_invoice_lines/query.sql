SELECT
    invoiceId, COUNT(*)
FROM
    InvoiceLine
GROUP BY
    invoiceId;