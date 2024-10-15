SELECT
    invoiceId,
    SUM(unitPrice * quantity) AS price
FROM
    InvoiceLine
GROUP BY
    invoiceId;