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

const downloadFile = (fileName: string, dataString: string) => {
  const downloadLinkElem = document.createElement('a');
  downloadLinkElem.setAttribute('href', dataString);
  downloadLinkElem.setAttribute('download', fileName);
  document.body.appendChild(downloadLinkElem);

  // trigger download
  downloadLinkElem.click();

  downloadLinkElem.remove();
};

// Formats a value as a CSV cell (RFC 4180): strings and numbers as they are,
// objects as JSON, null as an empty cell. A cell containing a comma, a double
// quote or a line break is quoted, with its double quotes doubled.
const toCSVCell = (value: unknown) => {
  if (value === null || value === undefined) {
    return '';
  }
  const text =
    typeof value === 'object' ? JSON.stringify(value) : String(value);
  return /[",\r\n]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
};

export const convertRowsToCSV = (rows: Record<string, unknown>[] = []) => {
  const columns = Object.keys(rows[0] || {});
  const titleRowString = columns.map(toCSVCell).join(',');
  // values are looked up by column, so every row follows the header order
  const rowsString = rows
    .map((row) => columns.map((column) => toCSVCell(row[column])).join(','))
    .join('\n');

  const csvContent = `${titleRowString}\n${rowsString}`;
  const encodedCsvContent = encodeURIComponent(csvContent);
  const csvDataString = `data:text/csv;charset=utf-8,${encodedCsvContent}`;
  return csvDataString;
};

export const downloadObjectAsCsvFile = (
  fileName: string,
  rows: Record<string, unknown>[] = [],
) => {
  if (!rows) {
    return false;
  }
  const fileNameWithSuffix = `${fileName}.csv`;

  const csvDataString = convertRowsToCSV(rows);

  downloadFile(fileNameWithSuffix, csvDataString);
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

  const dataString = convertRowsToJSON(object);

  downloadFile(fileNameWithSuffix, dataString);
  return true;
};
