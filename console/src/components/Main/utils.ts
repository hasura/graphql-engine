import { setLSItem, getLSItem } from '../../utils/localstorage';

const loveConsentState = 'console:loveIcon';
const proClickState = 'console:pro';
const defaultState = {
  isDismissed: false,
};
const defaultProClickState = {
  isProClicked: false,
};

const setLoveConsentState = (stateData: { isDismissed: boolean }) => {
  setLSItem(loveConsentState, JSON.stringify(stateData));
};

const getLoveConsentState = () => {
  const s = getLSItem(loveConsentState);

  if (s) {
    return JSON.parse(s);
  }

  setLSItem(loveConsentState, JSON.stringify(defaultState));

  return defaultState;
};

const setProClickState = (proStateData: { isProClicked: boolean }) => {
  setLSItem(proClickState, JSON.stringify(proStateData));
};

const getProClickState = () => {
  try {
    const proState = getLSItem(proClickState);

    if (proState) {
      return JSON.parse(proState);
    }

    setLSItem(proClickState, JSON.stringify(defaultProClickState));

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
