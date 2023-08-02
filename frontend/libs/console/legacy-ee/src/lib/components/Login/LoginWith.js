import React from 'react';
import { Button } from '@hasura/console-legacy-ce';

import { initiateOAuthRequest } from './utils';
import { clearAdminSecretState } from '../AppState';

import whiteLamda from './white-lamda.svg';
import styles from './Login.module.scss';

const LoginWith = props => {
  const { location, children, shouldRedirectBack } = props;

  const onClick = () => {
    clearAdminSecretState();
    initiateOAuthRequest(location, shouldRedirectBack);
  };

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
