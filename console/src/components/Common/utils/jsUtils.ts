// TODO: make functions from this file available without imports
import React from 'react';

import { showErrorNotification } from '../../Services/Common/Notification';

type Json =
  | null
  | boolean
  | number
  | string
  | Json[]
  | { [prop: string]: Json };

export const exists = (value: unknown) => {
  return value !== null && value !== undefined;
};

export const isArray = (value: unknown): value is Array<any> => {
  return Array.isArray(value);
};

export const isObject = (value: unknown): value is Record<PropertyKey, any> => {
  return typeof value === 'object' && value !== null;
};

export const isString = (value: unknown): value is string => {
  return typeof value === 'string';
};

export const isPromise = (value: unknown): value is Promise<any> => {
  if (!value) return false;
  return (value as Promise<any>).constructor.name === 'Promise';
};

export const isEmpty = (value: unknown) => {
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

export const isEqual = (value1: Json, value2: Json) => {
  let equal = false;

  if (typeof value1 === typeof value2) {
    if (isArray(value1)) {
      equal = JSON.stringify(value1) === JSON.stringify(value2);
    } else if (isObject(value2)) {
      const value1Keys = Object.keys(value1 as Exclude<Json, null>);
      const value2Keys = Object.keys(value2 as Exclude<Json, null>);

      if (value1Keys.length === value2Keys.length) {
        equal = true;

        for (let i = 0; i < value1Keys.length; i++) {
          const key = value1Keys[i];
          if (
            !isEqual(
              (value1 as Record<PropertyKey, any>)[key],
              (value2 as Record<PropertyKey, any>)[key]
            )
          ) {
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

export function getAllJsonPaths(
  json: Json,
  leafKeys: string[] = [],
  prefix = ''
) {
  const paths = [];

  const addPrefix = (subPath: string) => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson: Json, newPrefix: string) => {
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
    paths.push(addPrefix(json as string));
  }

  return paths;
}

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
      promptResponse = window.prompt(modalContent);

      isConfirmed = promptResponse === confirmationText;
    }
  }

  return isConfirmed;
};

export const uploadFile = (
  fileHandler: (s: string | ArrayBufferLike | null) => void,
  fileFormat: string | null,
  invalidFileHandler: any
) => (dispatch: any) => {
  const fileInputElement = document.createElement('div');
  fileInputElement.innerHTML = '<input style="display:none" type="file">';
  const fileInput = fileInputElement.firstChild as HTMLElement;
  document.body.appendChild(fileInputElement);

  const onFileUpload = () => {
    const file = (fileInput as HTMLInputElement)!.files![0];
    const fileName = file.name;

    let isValidFile = true;
    if (fileFormat) {
      const expectedFileSuffix = `.${fileFormat}`;

      if (!fileName.endsWith(expectedFileSuffix)) {
        isValidFile = false;

        if (invalidFileHandler) {
          invalidFileHandler(fileName);
        } else {
          // TODO
          dispatch(
            showErrorNotification(
              `Invalid file format`,
              `Expected a ${expectedFileSuffix} file`
            )
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

export const downloadObjectAsJsonFile = (fileName: string, object: Json) => {
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

export const getFileExtensionFromFilename = (filename: string) => {
  const matches = filename.match(/\.[0-9a-z]+$/i);
  return matches ? matches[0] : null;
};

// return time in format YYYY_MM_DD_hh_mm_ss_s
export const getCurrTimeForFileName = () => {
  const currTime = new Date();

  const year = currTime
    .getFullYear()
    .toString()
    .padStart(4, '0');

  const month = (currTime.getMonth() + 1).toString().padStart(2, '0');

  const day = currTime
    .getDate()
    .toString()
    .padStart(2, '0');

  const hours = currTime
    .getHours()
    .toString()
    .padStart(2, '0');

  const minutes = currTime
    .getMinutes()
    .toString()
    .padStart(2, '0');

  const seconds = currTime
    .getSeconds()
    .toString()
    .padStart(2, '0');

  const milliSeconds = currTime
    .getMilliseconds()
    .toString()
    .padStart(3, '0');

  return [year, month, day, hours, minutes, seconds, milliSeconds].join('_');
};

export const isValidTemplateLiteral = (literal_: string) => {
  const literal = literal_.trim();
  if (!literal) return false;
  const templateStartIndex = literal.indexOf('{{');
  const templateEndEdex = literal.indexOf('}}');
  return templateStartIndex !== -1 && templateEndEdex > templateStartIndex + 2;
};

export const getUrlSearchParamValue = (param: string) => {
  const urlSearchParams = new URLSearchParams(window.location.search);
  return urlSearchParams.get(param);
};

export const getLastArrayElement = <T = any>(array: Array<T>) => {
  if (!array) return null;
  if (!array.length) return null;
  return array[array.length - 1];
};

export const getFirstArrayElement = <T = any>(array: Array<T>) => {
  if (!array) return null;
  return array[0];
};

export const getEventTargetValue = (e: React.BaseSyntheticEvent) => {
  return e.target.value;
};
