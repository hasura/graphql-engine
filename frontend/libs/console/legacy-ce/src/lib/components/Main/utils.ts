import { jwtDecode } from 'jwt-decode';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import type { NotificationsState } from '@hasura/metadata/api';
import { LS_KEYS } from '@hasura/shared/types';

const defaultProClickState = {
  isProClicked: false,
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
    const decodedToken: DecodedJWT = jwtDecode(jwtToken);
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
  getReadAllNotificationsState,
  getUserType,
  setProClickState,
};
