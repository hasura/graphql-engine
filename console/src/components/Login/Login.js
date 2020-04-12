import React, { useState } from 'react';
import { push } from 'react-router-redux';
import Helmet from 'react-helmet';

import Button from '../Common/Button/Button';
import globals from '../../Globals';
import { verifyLogin } from './Actions';
import { CLI_CONSOLE_MODE } from '../../constants';
import { getAdminSecret } from '../Services/ApiExplorer/ApiRequest/utils';
import { Spinner, Text } from '../UIKit/atoms';
import styles from './Login.scss';

const hasuraLogo = require('./blue-logo.svg');

const Login = ({ dispatch }) => {
  // request state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  // should persist admin secret
  const [shouldPersist, setShouldPersist] = useState(true);

  // input handler
  const [adminSecretInput, setAdminSecretInput] = useState('');

  const getLoginForm = () => {
    const getLoginButtonText = () => {
      // login button text
      let loginText = 'Enter';

      if (loading) {
        loginText = (
          <Text>
            Verifying...
            <Spinner ml="sm" size="sm" display="inline-block" />
          </Text>
        );
      } else if (error) {
        loginText = 'Error. Try again?';
      }

      return loginText;
    };

    const toggleShouldPersist = () => setShouldPersist(!shouldPersist);

    const onAdminSecretChange = e => setAdminSecretInput(e.target.value);

    // form submit handler
    const onSubmit = e => {
      e.preventDefault();

      const successCallback = () => {
        setLoading(false);
        setError(null);
        dispatch(push(globals.urlPrefix));
      };

      const errorCallback = err => {
        setAdminSecretInput('');
        setLoading(false);
        setError(err);
      };

      setLoading(true);

      verifyLogin({
        adminSecret: adminSecretInput,
        shouldPersist,
        successCallback,
        errorCallback,
        dispatch
      });
    };

    return (
      <form className="form-horizontal" onSubmit={onSubmit}>
        <div className={styles.input_addon_group + ' ' + styles.padd_top}>
          <div className={'input-group ' + styles.input_group}>
            <input
              onChange={onAdminSecretChange}
              className={styles.form_input + ' form-control'}
              type="password"
              placeholder={'Enter admin-secret'}
              name="password"
            />
          </div>
        </div>
        <div className={styles.login_btn}>
          <Button
            type="submit"
            color="green"
            className="form-control"
            disabled={loading}
          >
            {getLoginButtonText()}
          </Button>
        </div>
        <div className={styles.add_pad_left}>
          <label className={`${styles.cursorPointer}`}>
            <input
              type="checkbox"
              checked={shouldPersist}
              onChange={toggleShouldPersist}
              className={`${styles.add_mar_right_small} ${styles.remove_margin_top} ${styles.cursorPointer}`}
            />
            Remember in this browser
          </label>
        </div>
      </form>
    );
  };

  const getCLIAdminSecretErrorMessage = () => {
    const adminSecret = getAdminSecret();

    const missingAdminSecretMessage = (
      <Text>
        Seems like your Hasura GraphQL engine instance has an admin-secret
        configured.
        <br />
        Run console with the admin-secret using:
        <br />
        <br />
        hasura console --admin-secret=&lt;your-admin-secret&gt;
      </Text>
    );

    const invalidAdminSecretMessage = (
      <Text>Invalid admin-secret passed from CLI</Text>
    );

    return (
      <div className={styles.text_center}>
        {adminSecret ? invalidAdminSecretMessage : missingAdminSecretMessage}
      </div>
    );
  };

  return (
    <div className={styles.mainWrapper + ' container-fluid'}>
      <div className={styles.container + ' container'} id="login">
        <div className={styles.loginCenter}>
          <Helmet title={'Login | ' + 'Hasura'} />
          <div className={styles.hasuraLogo}>
            <img src={hasuraLogo} />
          </div>
          <div className={styles.loginWrapper}>
            {globals.consoleMode !== CLI_CONSOLE_MODE
              ? getLoginForm()
              : getCLIAdminSecretErrorMessage()}
          </div>
        </div>
      </div>
    </div>
  );
};

const generatedLoginConnector = connect => {
  return connect()(Login);
};

export default generatedLoginConnector;
