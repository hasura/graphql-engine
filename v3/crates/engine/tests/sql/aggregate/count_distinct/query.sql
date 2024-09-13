SELECT
    COUNT(DISTINCT invoiceId) AS distinct_invoices,
    COUNT(1) AS num_invoice_lines
FROM
    InvoiceLine;