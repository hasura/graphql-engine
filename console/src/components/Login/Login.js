import React, { useState } from 'react';
import Helmet from 'react-helmet';
import Button from '../Common/Button/Button';
import globals from '../../Globals';
import { verifyLogin } from './Actions';

const styles = require('./Login.scss');
const hasuraLogo = require('./blue-logo.svg');

const Login = ({ dispatch }) => {
  // request state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  // should persist admin secret
  const [shouldPersist, setShouldPersist] = useState(true);
  const toggleShouldPersist = () => setShouldPersist(!shouldPersist);

  // input handler
  const [adminSecretInput, setAdminSecretInput] = useState('');
  const onAdminSecretChange = e => setAdminSecretInput(e.target.value);

  // login button text
  let loginText = 'Enter';
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

  // form submit handler
  const onSubmit = e => {
    e.preventDefault();
    const successCallback = () => {
      setLoading(false);
      setError(null);
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
      dispatch,
    });
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
            <form className="form-horizontal" onSubmit={onSubmit}>
              <div className={styles.input_addon_group + ' ' + styles.padd_top}>
                <div className={'input-group ' + styles.input_group}>
                  <input
                    onChange={onAdminSecretChange}
                    className={styles.form_input + ' form-control'}
                    type="password"
                    placeholder={`Enter ${globals.adminSecretLabel}`}
                    name="password"
                  />
                </div>
              </div>
              <div className={styles.signin_btn}>
                <Button
                  type="submit"
                  color="green"
                  className="form-control"
                  disabled={loading}
                >
                  {loginText}
                </Button>
              </div>
              <div className={`${styles.display_flex} ${styles.add_pad_left}`}>
                <input
                  type="checkbox"
                  checked={shouldPersist}
                  onChange={toggleShouldPersist}
                  className={`${styles.add_mar_right_small} ${
                    styles.remove_margin_top
                  }`}
                />
                <label>Remember in this browser</label>
              </div>
            </form>
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
