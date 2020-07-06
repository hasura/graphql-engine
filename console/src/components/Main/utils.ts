import { setLSItem, getLSItem, lsKeys } from '../../utils/localStorage';

const defaultState = {
  isDismissed: false,
};
const defaultProClickState = {
  isProClicked: false,
};

const setLoveConsentState = (stateData: { isDismissed: boolean }) => {
  setLSItem(lsKeys.loveConsent, JSON.stringify(stateData));
};

const getLoveConsentState = () => {
  const s = getLSItem(lsKeys.loveConsent);

  if (s) {
    return JSON.parse(s);
  }

  setLSItem(lsKeys.loveConsent, JSON.stringify(defaultState));

  return defaultState;
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  setLSItem(lsKeys.proClick, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const proState = getLSItem(lsKeys.proClick);

    if (proState) {
      return JSON.parse(proState);
    }

    setLSItem(lsKeys.proClick, JSON.stringify(defaultProClickState));

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
