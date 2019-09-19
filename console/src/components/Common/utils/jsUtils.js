// TODO: make functions from this file available without imports

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
      // TODO
    } else if (isObject(value2)) {
      _isEqual = JSON.stringify(value1) === JSON.stringify(value2);
    } else {
      _isEqual = value1 === value2;
    }
  }

  return _isEqual;
};

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
