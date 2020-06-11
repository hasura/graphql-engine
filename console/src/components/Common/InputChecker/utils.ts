const intChecker = (val: string) => {
  const rVal = parseInt(val, 10);

  if (!rVal) {
    throw new Error('Invalid input for type integer');
  }
  return rVal;
};

const boolChecker = (val: string) => {
  let rVal: boolean | null;
  if (val === 'true') {
    rVal = true;
  } else if (val === 'false') {
    rVal = false;
  } else {
    rVal = null;
  }

  if (rVal === null) {
    throw new Error('Invalid input for type bool');
  }
  return rVal;
};

const jsonChecker = (val: string) => {
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

type InputType = keyof typeof typeChecker;
const inputChecker = (type: string, value: string) => {
  // Checks the input against the intended type
  // and returns the value or error
  return new Promise((resolve, reject) => {
    try {
      if (type in typeChecker) {
        // as it's available in the object, the type can be added here explicitly without issues
        const inputType = type as InputType;
        resolve(typeChecker[inputType](value));
        return;
      }
      resolve(value);
    } catch (e) {
      reject(e);
    }
  });
};

export { inputChecker };
