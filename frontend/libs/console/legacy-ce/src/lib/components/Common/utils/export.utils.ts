import isObject from 'lodash.isobject';

export { getCurrTimeForFileName } from './jsUtils';

export function isJsonString(str: string) {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }
  return true;
}

const downloadFile = (fileName: string, dataString: string) => {
  const downloadLinkElem = document.createElement('a');
  downloadLinkElem.setAttribute('href', dataString);
  downloadLinkElem.setAttribute('download', fileName);
  document.body.appendChild(downloadLinkElem);

  // trigger download
  downloadLinkElem.click();

  downloadLinkElem.remove();
};

export const convertRowsToCSV = (rows: Record<string, unknown>[] = []) => {
  const titleRowString = Object.keys(rows[0] || {}).join(',');
  const rowsString = rows
    .map(e =>
      Object.values(e)
        .map(
          i =>
            `"${
              typeof i === 'string' && isJsonString(i)
                ? i.replace(/"/g, `'`) // in csv, a cell with double quotes and comma will result in bad formatting
                : JSON.stringify(i, null, 2).replace(/"/g, `'`)
            }"`
        )
        .join(',')
    )
    .join('\n');

  const csvContent = `${titleRowString}\n${rowsString}`;
  const encodedCsvContent = encodeURIComponent(csvContent);
  const csvDataString = `data:text/csv;charset=utf-8,${encodedCsvContent}`;
  return csvDataString;
};

export const downloadObjectAsCsvFile = (
  fileName: string,
  rows: Record<string, unknown>[] = []
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
    JSON.stringify(object, null, 2)
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
