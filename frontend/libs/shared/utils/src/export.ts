import isObject from 'lodash/isObject';

// return time in format YYYY_MM_DD_hh_mm_ss_s
export const getCurrTimeForFileName = () => {
  const currTime = new Date();

  const year = currTime.getFullYear().toString().padStart(4, '0');

  const month = (currTime.getMonth() + 1).toString().padStart(2, '0');

  const day = currTime.getDate().toString().padStart(2, '0');

  const hours = currTime.getHours().toString().padStart(2, '0');

  const minutes = currTime.getMinutes().toString().padStart(2, '0');

  const seconds = currTime.getSeconds().toString().padStart(2, '0');

  const milliSeconds = currTime.getMilliseconds().toString().padStart(3, '0');

  return [year, month, day, hours, minutes, seconds, milliSeconds].join('_');
};

const clickDownloadLink = (fileName: string, href: string) => {
  const downloadLinkElem = document.createElement('a');
  downloadLinkElem.setAttribute('href', href);
  downloadLinkElem.setAttribute('download', fileName);
  document.body.appendChild(downloadLinkElem);

  // trigger download
  downloadLinkElem.click();

  downloadLinkElem.remove();
};

// Downloads through a Blob URL rather than a `data:` URL: no URI-encoding
// pass over the whole file, and no browser size limit on large exports.
const downloadFile = (fileName: string, content: string, type: string) => {
  const url = URL.createObjectURL(new Blob([content], { type }));
  clickDownloadLink(fileName, url);
  // revoke once the click has handed the URL to the download manager
  setTimeout(() => URL.revokeObjectURL(url), 0);
};

export type CSVLineEnding = 'LF' | 'CRLF';

export type CSVExportOptions = {
  /** Separator between cells, e.g. `,`, `;` or a tab. */
  delimiter: string;
  /** Write the column names as the first row. */
  includeHeader: boolean;
  /** Start the file with a UTF-8 byte order mark, so Excel reads it as UTF-8. */
  includeBOM: boolean;
  lineEnding: CSVLineEnding;
  /** Quote every cell, not only those that need it. */
  quoteAllCells: boolean;
  /**
   * Prefix text cells starting with `=`, `+`, `-`, `@`, a tab or a carriage
   * return with `'`, so spreadsheets don't run them as formulas.
   */
  escapeFormulas: boolean;
  /** Text written for null values. */
  nullValue: string;
};

export const DEFAULT_CSV_EXPORT_OPTIONS: CSVExportOptions = {
  delimiter: ',',
  includeHeader: true,
  includeBOM: false,
  lineEnding: 'LF',
  quoteAllCells: false,
  escapeFormulas: false,
  nullValue: '',
};

const UTF8_BOM = '﻿';

const FORMULA_PREFIX = /^[=+\-@\t\r]/;

const escapeRegExp = (text: string) =>
  text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

// Formats a value as a CSV cell (RFC 4180): strings and numbers as they are,
// objects as JSON, null as `nullValue`. A cell containing the delimiter, a
// double quote or a line break is quoted, with its double quotes doubled.
const makeCSVCellFormatter = (options: CSVExportOptions) => {
  const needsQuotes = new RegExp(
    `["\\r\\n]|${escapeRegExp(options.delimiter)}`,
  );

  return (value: unknown) => {
    let text: string;
    if (value === null || value === undefined) {
      text = options.nullValue;
    } else if (typeof value === 'object') {
      text = JSON.stringify(value);
    } else {
      text = String(value);
    }

    // only text values: a negative number is data, not a formula
    if (
      options.escapeFormulas &&
      typeof value === 'string' &&
      FORMULA_PREFIX.test(text)
    ) {
      text = `'${text}`;
    }

    return options.quoteAllCells || needsQuotes.test(text)
      ? `"${text.replace(/"/g, '""')}"`
      : text;
  };
};

/**
 * Builds the CSV text for `rows`. The columns are the keys of the first row,
 * or `fallbackColumns` when there are no rows, so an empty export still has
 * its header.
 */
export const buildCSV = (
  rows: Record<string, unknown>[] = [],
  options: Partial<CSVExportOptions> = {},
  fallbackColumns: string[] = [],
) => {
  const opts = { ...DEFAULT_CSV_EXPORT_OPTIONS, ...options };
  const toCSVCell = makeCSVCellFormatter(opts);
  const lineBreak = opts.lineEnding === 'CRLF' ? '\r\n' : '\n';

  const columns = rows.length ? Object.keys(rows[0]) : fallbackColumns;
  const toLine = (cells: unknown[]) =>
    cells.map(toCSVCell).join(opts.delimiter);

  const lines = [
    ...(opts.includeHeader ? [toLine(columns)] : []),
    // values are looked up by column, so every row follows the header order
    ...rows.map((row) => toLine(columns.map((column) => row[column]))),
  ];

  return `${opts.includeBOM ? UTF8_BOM : ''}${lines.join(lineBreak)}`;
};

export const convertRowsToCSV = (
  rows: Record<string, unknown>[] = [],
  options: Partial<CSVExportOptions> = {},
) => {
  const encodedCsvContent = encodeURIComponent(buildCSV(rows, options));
  return `data:text/csv;charset=utf-8,${encodedCsvContent}`;
};

export const downloadObjectAsCsvFile = (
  fileName: string,
  rows: Record<string, unknown>[] = [],
  options: Partial<CSVExportOptions> = {},
  fallbackColumns: string[] = [],
) => {
  if (!rows) {
    return false;
  }
  const fileNameWithSuffix = `${fileName}.csv`;

  downloadFile(
    fileNameWithSuffix,
    buildCSV(rows, options, fallbackColumns),
    'text/csv;charset=utf-8',
  );
  return true;
};

export const convertRowsToJSON = (object: any) => {
  const contentType = 'application/json;charset=utf-8;';

  const dataString = `data:${contentType},${encodeURIComponent(
    JSON.stringify(object, null, 2),
  )}`;

  return dataString;
};

export const downloadObjectAsJsonFile = (fileName: string, object: any) => {
  if (!isObject(object)) {
    return false;
  }

  const jsonSuffix = '.json';

  const fileNameWithSuffix = fileName.endsWith(jsonSuffix)
    ? fileName
    : fileName + jsonSuffix;

  downloadFile(
    fileNameWithSuffix,
    JSON.stringify(object, null, 2),
    'application/json;charset=utf-8',
  );
  return true;
};
