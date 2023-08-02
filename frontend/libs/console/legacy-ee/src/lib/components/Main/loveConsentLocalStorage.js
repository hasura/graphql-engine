import { getLSItem, setLSItem, LS_KEYS } from '@hasura/console-legacy-ce';

const defaultState = {
  isDismissed: false,
};

const setLoveConsentState = stateData => {
  setLSItem(LS_KEYS.loveConsent, JSON.stringify(stateData));
};

const getLoveConsentState = stateData => {
  const s = getLSItem(LS_KEYS.loveConsent);
  if (s) {
    return JSON.parse(s);
  }
  setLoveConsentState(defaultState);
  return defaultState;
};

export { getLoveConsentState, setLoveConsentState };
