const loveConsentState = 'console:loveIcon';
const proConsentState = 'console:pro';
const defaultState = {
  isDismissed: false,
};
const defaultStatePro = {
  isProClicked: false,
}
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

const setproConsentState = proStateData => {
  window.localStorage.setItem(proConsentState, JSON.stringify(proStateData));
}
const getproConsentState = proStateData => {
  const p = window.localStorage.getItem(
    proConsentState,
    JSON.stringify(proStateData)
  );
  if (p) {
    return JSON.parse(p);
  }
  window.localStorage.setItem(proConsentState, JSON.stringify(defaultStatePro));
  return defaultStatePro;
}
export { getLoveConsentState, setLoveConsentState, getproConsentState, setproConsentState };
