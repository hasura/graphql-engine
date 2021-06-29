import moment from 'moment';

// TODO: make functions from this file available without imports
/* TYPE utils */

export const isNotDefined = (value: unknown) => {
  return value === null || value === undefined;
};

/*
 * Deprecated: Use "isNull" instead
 */
export const exists = (value: unknown) => {
  return value !== null && value !== undefined;
};

export const isArray = (value: unknown) => {
  return Array.isArray(value);
};

export const isObject = (value: unknown) => {
  return typeof value === 'object' && value !== null;
};

export const isString = (value: unknown) => {
  return typeof value === 'string';
};

export const isNumber = (value: unknown) => {
  return typeof value === 'number';
};

export const isFloat = (value: unknown) => {
  return typeof value === 'number' && value % 1 !== 0;
};

export const isBoolean = (value: unknown) => {
  return typeof value === 'boolean';
};

export const isPromise = (value: any) => {
  if (!value) return false;
  return value.constructor.name === 'Promise';
};

export const isValidURL = (value: string) => {
  try {
    new URL(value);
  } catch {
    return false;
  }
  return true;
};

export const isURLTemplated = (value: string): boolean => {
  return /{{(\S+)}}/.test(value);
};

export const isValidTemplateLiteral = (literal_: string) => {
  const literal = literal_.trim();
  if (!literal) return false;
  const templateStartIndex = literal.indexOf('{{');
  const templateEndEdex = literal.indexOf('}}');
  return templateStartIndex !== -1 && templateEndEdex > templateStartIndex + 2;
};

export const isValidDate = (date: Date) => {
  try {
    date.toISOString();
  } catch {
    return false;
  }
  return true;
};

export const isEmpty = (value: any) => {
  let empty = false;

  if (!exists(value)) {
    empty = true;
  } else if (isArray(value)) {
    empty = value.length === 0;
  } else if (isObject(value)) {
    empty = JSON.stringify(value) === JSON.stringify({});
  } else if (isString(value)) {
    empty = value === '';
  }

  return empty;
};

export const isEqual = (value1: any, value2: any) => {
  let equal = false;

  if (typeof value1 === typeof value2) {
    if (isArray(value1)) {
      equal = JSON.stringify(value1) === JSON.stringify(value2);
    } else if (isObject(value2)) {
      const value1Keys = Object.keys(value1);
      const value2Keys = Object.keys(value2);

      if (value1Keys.length === value2Keys.length) {
        equal = true;

        for (let i = 0; i < value1Keys.length; i++) {
          const key = value1Keys[i];
          if (!isEqual(value1[key], value2[key])) {
            equal = false;
            break;
          }
        }
      }
    } else {
      equal = value1 === value2;
    }
  }

  return equal;
};

export function isJsonString(str: string) {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }
  return true;
}

export const isNumberString = (str: string | number) =>
  !Number.isNaN(Number(str));

export const isArrayString = (str: string) => {
  try {
    if (isJsonString(str) && Array.isArray(JSON.parse(str))) {
      return true;
    }
  } catch (e) {
    return false;
  }
  return false;
};

/* ARRAY utils */
export const deleteArrayElementAtIndex = (array: unknown[], index: number) => {
  return array.splice(index, 1);
};

export const getLastArrayElement = (array: unknown[]) => {
  return array[array.length - 1];
};

export const arrayDiff = (arr1: unknown[], arr2: unknown[]) => {
  return arr1.filter(v => !arr2.includes(v));
};

export const isStringArray = (str: string): boolean => {
  try {
    const arr = JSON.parse(str);
    return Array.isArray(arr);
  } catch {
    return false;
  }
};

/* JSON utils */

export function getAllJsonPaths(json: any, leafKeys: any[], prefix = '') {
  const paths = [];

  const addPrefix = (subPath: string) => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson: any, newPrefix: string) => {
    const subPaths = getAllJsonPaths(subJson, leafKeys, newPrefix);

    subPaths.forEach(subPath => {
      paths.push(subPath);
    });

    if (!subPaths.length) {
      paths.push(newPrefix);
    }
  };

  if (isArray(json)) {
    json.forEach((subJson: any, i: number) => {
      handleSubJson(subJson, addPrefix(i.toString()));
    });
  } else if (isObject(json)) {
    Object.keys(json).forEach(key => {
      if (leafKeys.includes(key)) {
        paths.push({ [addPrefix(key)]: json[key] });
      } else {
        handleSubJson(json[key], addPrefix(key));
      }
    });
  } else {
    paths.push(addPrefix(json));
  }

  return paths;
}

/* TRANSFORM utils */

export const capitalize = (s: string) => {
  return s.charAt(0).toUpperCase() + s.slice(1);
};

// return number with commas for readability
export const getReadableNumber = (number: number) => {
  return number.toLocaleString();
};

/* URL utils */

export const getUrlSearchParamValue = (param: string) => {
  const urlSearchParams = new URLSearchParams(window.location.search);
  return urlSearchParams.get(param);
};
/* ALERT utils */

// use browser confirm and prompt to get user confirmation for actions
export const getConfirmation = (
  message = '',
  hardConfirmation = false,
  confirmationText = 'CONFIRM'
) => {
  let isConfirmed = false;

  let modalContent = '';

  modalContent += 'Are you sure?';

  if (message) {
    modalContent += '\n\n';
    modalContent += message;
  }

  if (!hardConfirmation) {
    isConfirmed = window.confirm(modalContent);
  } else {
    modalContent += '\n\n';
    modalContent += `Type "${confirmationText}" to confirm:`;

    // retry prompt until user cancels or confirmation text matches
    // prompt returns null on cancel or a string otherwise
    let promptResponse: string | null = '';
    while (!isConfirmed && promptResponse !== null) {
      promptResponse = prompt(modalContent);

      isConfirmed = promptResponse === confirmationText;
    }
  }

  return isConfirmed;
};

/* FILE utils */

export const uploadFile = (
  fileHandler: (s: string | ArrayBufferLike | null) => void,
  fileFormat: string | null,
  invalidFileHandler: any,
  errorCallback?: (title: string, subTitle: string, details?: any) => void
) => {
  const fileInputElement = document.createElement('div');
  fileInputElement.innerHTML = '<input style="display:none" type="file">';
  const fileInput: any = fileInputElement.firstChild;
  document.body.appendChild(fileInputElement);

  const onFileUpload = () => {
    const file = fileInput.files[0];
    const fileName = file.name;

    let isValidFile = true;
    if (fileFormat) {
      const expectedFileSuffix = `.${fileFormat}`;

      if (!fileName.endsWith(expectedFileSuffix)) {
        isValidFile = false;

        if (invalidFileHandler) {
          invalidFileHandler(fileName);
        } else if (errorCallback) {
          errorCallback(
            'Invalid file format',
            `Expected a ${expectedFileSuffix} file`
          );
        }

        fileInputElement.remove();
      }
    }

    if (isValidFile) {
      const reader = new FileReader();

      // this function will execute once the reader has successfully read the file
      reader.onload = () => {
        fileInputElement.remove();

        fileHandler(reader.result);
      };

      reader.readAsText(file);
    }
  };

  // attach file upload handler
  fileInput.addEventListener('change', onFileUpload);

  // trigger file upload window
  fileInput.click();
};

export const downloadFile = (fileName: string, dataString: string) => {
  const downloadLinkElem = document.createElement('a');
  downloadLinkElem.setAttribute('href', dataString);
  downloadLinkElem.setAttribute('download', fileName);
  document.body.appendChild(downloadLinkElem);

  // trigger download
  downloadLinkElem.click();

  downloadLinkElem.remove();
};

export const downloadObjectAsJsonFile = (fileName: string, object: any) => {
  const contentType = 'application/json;charset=utf-8;';

  const jsonSuffix = '.json';
  const fileNameWithSuffix = fileName.endsWith(jsonSuffix)
    ? fileName
    : fileName + jsonSuffix;

  const dataString = `data:${contentType},${encodeURIComponent(
    JSON.stringify(object, null, 2)
  )}`;

  downloadFile(fileNameWithSuffix, dataString);
};
export const downloadObjectAsCsvFile = (
  fileName: string,
  rows: Record<string, unknown>[] = []
) => {
  const titleRowString = Object.keys(rows[0]).join(',');
  const rowsString = rows
    .map(e =>
      Object.values(e)
        .map(
          i =>
            `"${
              typeof i === 'string' && isJsonString(i)
                ? i.replace(/"/g, `'`) // in csv, a cell with double quotes and comma will result is bad formating
                : JSON.stringify(i, null, 2).replace(/"/g, `'`)
            }"`
        )
        .join(',')
    )
    .join('\n');
  const csvContent = `data:text/csv;charset=utf-8,${titleRowString}\n${rowsString}`;

  const fileNameWithSuffix = `${fileName}.csv`;

  const encodedUri = encodeURI(csvContent);

  downloadFile(fileNameWithSuffix, encodedUri);
};
export const getFileExtensionFromFilename = (filename: string) => {
  const matches = filename.match(/\.[0-9a-z]+$/i);
  return matches ? matches[0] : null;
};

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

export const convertDateTimeToLocale = (dateTime: string | Date | number) => {
  return moment(dateTime, moment.ISO_8601).format('ddd, MMM Do HH:mm:ss Z');
};
