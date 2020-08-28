import JWTDecode from 'jwt-decode';
import { NotificationScope } from './ConsoleNotification';
import { Nullable } from '../Common/utils/tsUtils';

const proClickState = 'console:pro';
const loveConsentState = 'console:loveIcon';
const defaultProClickState = {
  isProClicked: false,
};
const defaultState = {
  isDismissed: false,
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

const getReadAllNotificationsState = () => {
  return {
    read: 'all',
    date: new Date().toISOString(),
    showBadge: false,
  };
};

const getConsoleScope = (
  serverVersion: string,
  consoleID: Nullable<string>,
): NotificationScope => {
  if (!consoleID) {
    return 'OSS';
  }

  if (serverVersion.includes('cloud')) {
    return 'CLOUD';
  }

  if (serverVersion.includes('pro')) {
    return 'PRO';
  }

  return 'OSS';
};

// This function is specifically to help identify the multiple users on cloud.
// This is a temporary solution atm. but improvements will be added soon
const getUserType = (token: string) => {
  const IDToken = 'IDToken ';
  if (!token.includes(IDToken)) {
    return 'admin';
  }
  const jwtToken = token.split(IDToken)[1];
  try {
    const decodedToken: { sub?: string } = JWTDecode(jwtToken);
    if (!decodedToken.sub) {
      return 'admin';
    }
    return decodedToken.sub;
  } catch {
    return 'admin';
  }
};

export {
  getProClickState,
  setProClickState,
  setLoveConsentState,
  getLoveConsentState,
  getReadAllNotificationsState,
  getConsoleScope,
  getUserType,
};
