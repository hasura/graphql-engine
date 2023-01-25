const intChecker = val => {
  const rVal = parseInt(val, 10);

  if (!rVal) {
    throw new Error('Invalid input for type integer');
  }
  return rVal;
};
const boolChecker = val => {
  let rVal = '';
  if (val === 'true') {
    rVal = true;
  } else if (rVal === 'false') {
    rVal = false;
  } else {
    rVal = null;
  }

  if (rVal === null) {
    throw new Error('Invalid input for type bool');
  }
  return rVal;
};

const jsonChecker = val => {
  try {
    JSON.parse(val);
  } catch (e) {
    throw e;
  }
  return val;
};

const typeChecker = {
  integer: intChecker,
  numeric: intChecker,
  bigint: intChecker,
  boolean: boolChecker,
  json: jsonChecker,
  jsonb: jsonChecker,
};

const inputChecker = (type, value) => {
  // Checks the input against the intended type
  // and returns the value or error
  return new Promise((resolve, reject) => {
    try {
      if (type in typeChecker) {
        resolve(typeChecker[type](value));
        return;
      }
      resolve(value);
    } catch (e) {
      reject(e);
    }
  });
};

export { inputChecker };
