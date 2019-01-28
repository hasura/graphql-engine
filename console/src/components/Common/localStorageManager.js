const loveConsentState = 'console:loveIcon';
const defaultState = {
  isDismissed: false,
};

const setLoveConsentState = stateData => {
  window.localStorage.setItem(loveConsentState, JSON.stringify(stateData));
};

const getLoveConsentState = stateData => {
  const s = window.localStorage.getItem(
    loveConsentState,
    JSON.stringify(stateData)
  );
  if (s) {
    return JSON.parse(s);
  }
  window.localStorage.setItem(loveConsentState, JSON.stringify(defaultState));
  return defaultState;
};

export { getLoveConsentState, setLoveConsentState };
