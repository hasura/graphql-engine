// TODO: make functions from this file available without imports

import { showErrorNotification } from '../../Services/Common/Notification';

export const exists = value => {
  return value !== null && value !== undefined;
};

export const isArray = value => {
  return Array.isArray(value);
};

export const isObject = value => {
  return typeof value === 'object';
};

export const isString = value => {
  return typeof value === 'string';
};

export const isEmpty = value => {
  let _isEmpty = false;

  if (!exists(value)) {
    _isEmpty = true;
  } else if (isArray(value)) {
    _isEmpty = value.length === 0;
  } else if (isObject(value)) {
    _isEmpty = JSON.stringify(value) === JSON.stringify({});
  } else if (isString(value)) {
    _isEmpty = value === '';
  }

  return _isEmpty;
};

export const isEqual = (value1, value2) => {
  let _isEqual = false;

  if (typeof value1 === typeof value2) {
    if (isArray(value1)) {
      _isEqual = JSON.stringify(value1) === JSON.stringify(value2);
    } else if (isObject(value2)) {
      const value1Keys = Object.keys(value1);
      const value2Keys = Object.keys(value2);

      if (value1Keys.length === value2Keys.length) {
        _isEqual = true;

        for (let i = 0; i < value1Keys.length; i++) {
          const key = value1Keys[i];
          if (!isEqual(value1[key], value2[key])) {
            _isEqual = false;
            break;
          }
        }
      }
    } else {
      _isEqual = value1 === value2;
    }
  }

  return _isEqual;
};

export function isJsonString(str) {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }

  return true;
}

export function getAllJsonPaths(json, leafKeys = [], prefix = '') {
  const _paths = [];

  const addPrefix = subPath => {
    return prefix + (prefix && subPath ? '.' : '') + subPath;
  };

  const handleSubJson = (subJson, newPrefix) => {
    const subPaths = getAllJsonPaths(subJson, leafKeys, newPrefix);

    subPaths.forEach(subPath => {
      _paths.push(subPath);
    });

    if (!subPaths.length) {
      _paths.push(newPrefix);
    }
  };

  if (isArray(json)) {
    json.forEach((subJson, i) => {
      handleSubJson(subJson, addPrefix(i.toString()));
    });
  } else if (isObject(json)) {
    Object.keys(json).forEach(key => {
      if (leafKeys.includes(key)) {
        _paths.push({ [addPrefix(key)]: json[key] });
      } else {
        handleSubJson(json[key], addPrefix(key));
      }
    });
  } else {
    _paths.push(addPrefix(json));
  }

  return _paths;
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
    isConfirmed = confirm(modalContent);
  } else {
    modalContent += '\n\n';
    modalContent += `Type "${confirmationText}" to confirm:`;

    // retry prompt until user cancels or confirmation text matches
    // prompt returns null on cancel or a string otherwise
    let promptResponse = '';
    while (!isConfirmed && promptResponse !== null) {
      promptResponse = prompt(modalContent);

      isConfirmed = promptResponse === confirmationText;
    }
  }

  return isConfirmed;
};

export const uploadFile = (
  fileHandler,
  fileFormat = null,
  invalidFileHandler = null
) => dispatch => {
  const fileInputElement = document.createElement('div');
  fileInputElement.innerHTML = '<input style="display:none" type="file">';
  const fileInput = fileInputElement.firstChild;
  document.body.appendChild(fileInputElement);

  const onFileUpload = () => {
    const file = fileInput.files[0];
    const fileName = file.name;

    let isValidFile = true;
    if (fileFormat) {
      const expectedFileSuffix = '.' + fileFormat;

      if (!fileName.endsWith(expectedFileSuffix)) {
        isValidFile = false;

        if (invalidFileHandler) {
          invalidFileHandler(fileName);
        } else {
          dispatch(
            showErrorNotification(
              'Invalid file format',
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

export const downloadFile = (fileName, dataString) => {
  const downloadLinkElem = document.createElement('a');
  downloadLinkElem.setAttribute('href', dataString);
  downloadLinkElem.setAttribute('download', fileName);
  document.body.appendChild(downloadLinkElem);

  // trigger download
  downloadLinkElem.click();

  downloadLinkElem.remove();
};

export const downloadObjectAsJsonFile = (fileName, object) => {
  const contentType = 'application/json;charset=utf-8;';

  const jsonSuffix = '.json';
  const fileNameWithSuffix = fileName.endsWith(jsonSuffix)
    ? fileName
    : fileName + jsonSuffix;

  const dataString =
    'data:' + contentType + ',' + encodeURIComponent(JSON.stringify(object));

  downloadFile(fileNameWithSuffix, dataString);
};
