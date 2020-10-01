import { setLSItem, getLSItem, LS_KEYS } from '../../utils/localStorage';

const defaultState = {
  isDismissed: false,
};
const defaultProClickState = {
  isProClicked: false,
};

const setLoveConsentState = (stateData: { isDismissed: boolean }) => {
  setLSItem(LS_KEYS.loveConsent, JSON.stringify(stateData));
};

const getLoveConsentState = () => {
  const s = getLSItem(LS_KEYS.loveConsent);

  if (s) {
    return JSON.parse(s);
  }

  setLSItem(LS_KEYS.loveConsent, JSON.stringify(defaultState));

  return defaultState;
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  setLSItem(LS_KEYS.proClick, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const proState = getLSItem(LS_KEYS.proClick);

    if (proState) {
      return JSON.parse(proState);
    }

    setLSItem(LS_KEYS.proClick, JSON.stringify(defaultProClickState));

    return defaultProClickState;
  } catch (err) {
    console.error(err);
    return defaultProClickState;
  }
};

export {
  getLoveConsentState,
  setLoveConsentState,
  getProClickState,
  setProClickState,
};
