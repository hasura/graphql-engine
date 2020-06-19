const INTERCOM_ID = 'rucirpb3';
export const INTERCOM_URL = `https://widget.intercom.io/widget/${INTERCOM_ID}`;

export const bootIntercom = () => {
  if (!window.Intercom) return;
  window.Intercom!('boot', {
    app_id: INTERCOM_ID,
  });
};

export const closeIntercom = () => {
  if (window.Intercom) window.Intercom('shutdown');
};

export const startIntercom = () => {
  if (window.Intercom) bootIntercom();

  const head = document.getElementsByTagName('head')[0];
  const script = document.createElement('script');
  script.type = 'text/javascript';
  script.onload = bootIntercom;
  script.src = INTERCOM_URL;
  head.appendChild(script);
};

const chatState = 'console:chatEnabled';

export const persistChatState = (isEnabled: boolean) => {
  window.localStorage.setItem(chatState, String(isEnabled));
};

export const getPersistedChatState = () => {
  const state = window.localStorage.getItem(chatState);
  try {
    if (state) {
      return JSON.parse(state);
    }
    return false;
  } catch {
    return false;
  }
};

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
