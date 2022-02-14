import React, { useState, ChangeEvent, FormEvent } from 'react';
import { Connect } from 'react-redux';
import { push } from 'react-router-redux';
import Helmet from 'react-helmet';
import Button from '../Common/Button/Button';
import globals from '../../Globals';
import { verifyLogin } from './Actions';
import { CLI_CONSOLE_MODE } from '../../constants';
import { getAdminSecret } from '../Services/ApiExplorer/ApiRequest/utils';
import { ConnectInjectedProps } from '../../types';

import styles from './Login.scss';
import hasuraLogo from './black-logo.svg';

const Login: React.FC<ConnectInjectedProps> = ({ dispatch }) => {
  // request state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  // should persist admin secret
  const [shouldPersist, setShouldPersist] = useState(true);

  // input handler
  const [adminSecretInput, setAdminSecretInput] = useState('');

  const getLoginForm = () => {
    const getLoginButtonText = () => {
      // login button text
      let loginText: React.ReactNode = 'Enter';
      if (loading) {
        loginText = (
          <span>
            Verifying...
            <i className="fa fa-spinner fa-spin" aria-hidden="true" />
          </span>
        );
      } else if (error) {
        loginText = 'Error. Try again?';
      }

      return loginText;
    };

    const toggleShouldPersist = () => setShouldPersist(!shouldPersist);

    const onAdminSecretChange = (e: ChangeEvent<HTMLInputElement>) =>
      setAdminSecretInput(e.target.value);

    // form submit handler
    const onSubmit = (e: FormEvent<HTMLFormElement>) => {
      e.preventDefault();

      const successCallback = () => {
        setLoading(false);
        setError(null);
        dispatch(push(globals.urlPrefix));
      };

      const errorCallback = (err: Error) => {
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
        dispatch,
      });
    };

    return (
      <form className="form-horizontal" onSubmit={onSubmit}>
        <div className={`${styles.input_addon_group} ${styles.padd_top}`}>
          <div className={`input-group ${styles.input_group}`}>
            <input
              onChange={onAdminSecretChange}
              className={`${styles.form_input} form-control`}
              type="password"
              placeholder="Enter admin-secret"
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
          <label className={`${styles.cursorPointer} flex items-center`}>
            <input
              type="checkbox"
              checked={shouldPersist}
              onChange={toggleShouldPersist}
              className={`${styles.add_mar_right_small} ${styles.remove_margin_top} ${styles.cursorPointer} legacy-input-fix`}
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
      <span>
        Seems like your Hasura GraphQL engine instance has an admin-secret
        configured.
        <br />
        Run console with the admin-secret using:
        <br />
        <br />
        hasura console --admin-secret=&lt;your-admin-secret&gt;
      </span>
    );

    const invalidAdminSecretMessage = (
      <span>Invalid admin-secret passed from CLI</span>
    );

    return (
      <div className={styles.text_center}>
        {adminSecret ? invalidAdminSecretMessage : missingAdminSecretMessage}
      </div>
    );
  };

  return (
    <div className={`${styles.mainWrapper} container-fluid`}>
      <div className={`${styles.container} container`} id="login">
        <div className={styles.loginCenter}>
          <Helmet title="Login | Hasura" />
          <div className={styles.hasuraLogo}>
            <img src={hasuraLogo} alt="Hasura" />
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

const generatedLoginConnector = (connect: Connect) => {
  return connect()(Login);
};

export default generatedLoginConnector;
