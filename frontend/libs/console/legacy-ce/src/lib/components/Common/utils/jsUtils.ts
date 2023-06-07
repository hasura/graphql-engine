import moment from 'moment';
import { isJsonString } from './export.utils';

export { isJsonString, getCurrTimeForFileName } from './export.utils';
// TODO: make functions from this file available without imports
/* TYPE utils */

export const isNotDefined = (value: unknown) => {
  return value === null || value === undefined;
};

/*
 * @deprecated use "isNull" instead
 */
export const exists = (value: unknown) => {
  return value !== null && value !== undefined;
};

export const isArray = (value: unknown): value is any[] => {
  return Array.isArray(value);
};

export const isObject = (value: unknown): value is Record<string, unknown> => {
  return typeof value === 'object' && value !== null;
};

export type TypedObjectValidator = (val: Record<string, unknown>) => boolean;

/*
  NOTE: this function is useful to assert on the type of an object and access it's properties
  See the tests for examples.
*/
export const isTypedObject = <T>(
  value: unknown,
  validator: TypedObjectValidator
): value is T => isObject(value) && validator(value);

export const isString = (value: unknown): value is string => {
  return typeof value === 'string';
};

export const isNumber = (value: unknown): value is number => {
  return typeof value === 'number';
};

export const isFloat = (value: unknown): value is number => {
  return typeof value === 'number' && value % 1 !== 0;
};

export const isBoolean = (value: unknown): value is boolean => {
  return typeof value === 'boolean';
};

export const isPromise = (value: any): value is typeof Promise => {
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

export function emptyStringToNull(val?: string): string | null {
  return val && val !== '' ? val : null;
}

// from https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest#converting_a_digest_to_a_hex_string
export const hashString = async (
  str: string,
  algorithm?: AlgorithmIdentifier
) => {
  const msgUint8 = new TextEncoder().encode(str); // encode as (utf-8) Uint8Array
  const hashBuffer = await crypto.subtle.digest(
    algorithm || 'SHA-256',
    msgUint8
  ); // hash the message
  const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
  const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join(''); // convert bytes to hex string
  return hashHex;
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

export function getAllJsonPaths(
  json: any,
  leafKeys: any[],
  prefix = ''
): Record<string, any>[] | string[] {
  const paths = [];

  const addPrefix = (subPath: string) => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson: any, newPrefix: string) => {
    const subPaths = getAllJsonPaths(subJson, leafKeys, newPrefix);

    subPaths.forEach((subPath: (typeof subPaths)[0]) => {
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

export const encodeFileContent = (data: string) => encodeURIComponent(data);

export const getFileExtensionFromFilename = (filename: string) => {
  const matches = filename.match(/\.[0-9a-z]+$/i);
  return matches ? matches[0] : null;
};

export const convertDateTimeToLocale = (dateTime: string | Date | number) => {
  return moment(dateTime, moment.ISO_8601).format(
    'ddd, MMM, yyyy, Do HH:mm:ss Z'
  );
};

export const isConsoleError = (x: any): x is Error => {
  return typeof x.name === 'string' && typeof x.message === 'string';
};
