const loveConsentState = 'console:loveIcon';
const proClickState = 'console:pro';
const defaultState = {
  isDismissed: false,
};
const defaultProClickState = {
  isProClicked: false,
};

const setLoveConsentState = (stateData: { isDismissed: boolean }) => {
  window.localStorage.setItem(loveConsentState, JSON.stringify(stateData));
};

const getLoveConsentState = () => {
  const s = window.localStorage.getItem(loveConsentState);

  if (s) {
    return JSON.parse(s);
  }

  window.localStorage.setItem(loveConsentState, JSON.stringify(defaultState));

  return defaultState;
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  window.localStorage.setItem(proClickState, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const p = window.localStorage.getItem(proClickState);

    if (p) {
      return JSON.parse(p);
    }

    window.localStorage.setItem(
      proClickState,
      JSON.stringify(defaultProClickState)
    );

    return defaultProClickState;
  } catch (e) {
    console.error(e);
    return defaultProClickState;
  }
};

/**
 * ## parseResponseWithBigInt
 * Utility to parse http response safely to json.
 * When we use fetch `.json()` v8 engine will round off the bigInt value to nearest safe number this utility will convert big ints to strings
 * This utility will change the type of all big integers to
 *
 * @param {string} res text input comes directly from the http request or any string json
 * @returns json object, all big ints are converted to string data type
 */
const parseResponseWithBigInt: object = (res: string) => {
  const reg = new RegExp(/(\d+)/gm);
  const k = res.replace(reg, x => {
    const num = Number.parseInt(x, 10);

    // easy exit
    if (Number.isNaN(num)) return x;
    // convert big int to string
    if (num > Number.MAX_SAFE_INTEGER) return `"${x}"`;
    if (num < Number.MIN_SAFE_INTEGER) return `"${x}"`;

    // all other numbers
    return x;
  });

  let result;
  try {
    result = JSON.parse(k);
  } catch (e) {
    // main thread will still work
    result = JSON.parse(res);
  }
  return result;
};

export {
  getLoveConsentState,
  setLoveConsentState,
  getProClickState,
  setProClickState,
  parseResponseWithBigInt,
};
