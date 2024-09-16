SELECT
    Invoice.*,
    COUNT(1) AS invoice_line_count
FROM
    Invoice
    JOIN InvoiceLine USING (invoiceId)
GROUP BY
    Invoice.invoiceId
LIMIT
    10;