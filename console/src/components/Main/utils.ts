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

export {
  getLoveConsentState,
  setLoveConsentState,
  getProClickState,
  setProClickState,
};
