import React from 'react';
import { Button } from '@hasura/console-oss';

import { getAuthorizeUrl, modifyRedirectUrl } from './utils';
import { hasOAuthLoggedIn } from '../OAuthCallback/utils';

import { parseQueryString } from '../../helpers/parseQueryString';
import { clearAdminSecretState } from '../AppState';

const whiteLamda = require('./white-lamda.svg');

const initiateOAuthRequest = (location, shouldRedirectBack) => {
  const parsed = parseQueryString(location.search);
  const authUrl = getAuthorizeUrl();
  hasOAuthLoggedIn(false);
  if (shouldRedirectBack) {
    modifyRedirectUrl(location.pathname);
  } else if (
    'redirect_url' in parsed &&
    parsed.redirect_url &&
    parsed.redirect_url !== 'undefined' &&
    parsed.redirect_url !== 'null'
  ) {
    modifyRedirectUrl(parsed.redirect_url);
  } else {
    modifyRedirectUrl('/');
  }

  window.location.href = authUrl;
};

const LoginWith = props => {
  const { location, children, shouldRedirectBack } = props;

  const onClick = () => {
    clearAdminSecretState();
    initiateOAuthRequest(location, shouldRedirectBack);
  };

  const styles = require('./Login.scss');
  if (children) {
    return (
      <a onClick={onClick} className={styles.cursorPointer}>
        {children}
      </a>
    );
  }

  return (
    <div className={styles.loginWithWrapper}>
      <div className={styles.signin_btn}>
        <Button type="button" className="form-control" onClick={onClick}>
          <div className={styles.signin_btn_img}>
            <img src={whiteLamda} alt={'Hasura icon'} />
          </div>
          <div className={styles.signin_btn_content}>LOGIN WITH HASURA</div>
        </Button>
      </div>
    </div>
  );
};

export default LoginWith;
