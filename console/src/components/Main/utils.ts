import jwt_decode from 'jwt-decode';

import { NotificationsState } from '../../telemetry/state';
import { getLSItem, LS_KEYS, setLSItem } from '../../utils/localStorage';
import { Nullable } from '../Common/utils/tsUtils';
import { ConsoleScope } from './ConsoleNotification';

const defaultProClickState = {
  isProClicked: false,
};
const defaultState = {
  isDismissed: false,
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

const getReadAllNotificationsState = (): NotificationsState => {
  return {
    read: 'all',
    date: new Date().toISOString(),
    showBadge: false,
  };
};

const getConsoleScope = (
  serverVersion: string,
  consoleID: Nullable<string>
): ConsoleScope => {
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

// added these, so that it can be repurposed if needed
type JWTKeys = 'sub' | 'iat' | 'aud' | 'exp' | 'iss';
type JWTType = Record<JWTKeys, string>;
type DecodedJWT = Partial<JWTType>;

// This function is specifically to help identify the multiple users on cloud.
// This is a temporary solution atm. but improvements will be added soon
const getUserType = (token: string) => {
  const IDToken = 'IDToken ';
  if (!token.includes(IDToken)) {
    return 'admin';
  }
  const jwtToken = token.split(IDToken)[1];
  try {
    const decodedToken: DecodedJWT = jwt_decode(jwtToken);
    if (!decodedToken.sub) {
      return 'admin';
    }
    return decodedToken.sub;
  } catch {
    return 'admin';
  }
};

export {
  getConsoleScope,
  getLoveConsentState,
  getProClickState,
  getReadAllNotificationsState,
  getUserType,
  setLoveConsentState,
  setProClickState,
};
