import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { FaSpinner } from 'react-icons/fa';
import globals from '../../Globals';
import {
  loginClicked,
  UPDATE_ADMIN_SECRET_INPUT,
  UPDATE_PERSONAL_ACCESS_TOKEN,
  patLoginClicked,
  UPDATE_PROJECT_ID,
  UPDATE_PROJECT_NAME,
} from '../Main/Actions';
import { IconTooltip } from '@hasura/console-legacy-ce';

// import { FT_LOGIN_WITH_HASURA } from '../../helpers/versionUtils';

import { getFromLS, initLS } from './localStorage';

import LoginWith from './LoginWith';
import { clearAdminSecretState } from '../AppState';

import { isClientSet, initiateOAuthRequest } from './utils';

import loginIcon from './login.svg';
import styles from './Login.module.scss';
import hasuraLogo from './black-logo.svg';
import hasuraEELogo from './mono-dark-ee.svg';
import ssoIcon from './black-building.svg';
import keyIcon from './black-key.svg';
import { AdminSecretLogin } from './AdminSecretLogin';
import { SSOLogin } from './SSOLogin';
import SSOLoginButton from './SSOLoginButton';

class Login extends Component {
  constructor(props) {
    super(props);
    if (globals.consoleMode === 'cli') {
      this.state = { loginMethod: 'access-token' };
    } else {
      this.state = { loginMethod: 'admin-secret' };
    }
    this.state = { ...this.state, showAdminSecretLogin: false };
  }

  componentDidMount() {
    const currentState = getFromLS();
    if (!currentState) {
      initLS();
    }
  }

  handleAdminSecret = e => {
    this.props.dispatch({
      type: UPDATE_ADMIN_SECRET_INPUT,
      data: e.target.value,
    });
  };

  handlePAT = e => {
    this.props.dispatch({
      type: UPDATE_PERSONAL_ACCESS_TOKEN,
      data: e.target.value,
    });
    this.props.dispatch({
      type: UPDATE_PROJECT_ID,
      data: globals.projectId,
    });
    this.props.dispatch({
      type: UPDATE_PROJECT_NAME,
      data: globals.projectName,
    });
  };

  render() {
    const {
      loginInProgress,
      dispatch,
      // featuresCompatibility,
      location,
    } = this.props;

    // const { [FT_LOGIN_WITH_HASURA]: isLoginWithHasura } = featuresCompatibility;

    const dropdownOptions = [
      { title: 'Admin Secret', value: 'admin-secret' },
      { title: 'Personal Access Token', value: 'access-token' },
    ];

    const renderLogin = () => {
      // Dont show "login with lux control plane" when running in pro-lite mode
      if (isClientSet() && globals.consoleType !== 'pro-lite') {
        return (
          <LoginWith location={location}>
            <div
              className={`${styles.cursorPointer} ${styles.signin_btn} ${styles.display_flex}`}
            >
              Or login with your Hasura Cloud account
            </div>
          </LoginWith>
        );
        // return <LoginWith location={location} />;
      }
    };

    // render SSO login buttons from 3rd-party identity providers
    // that EE users define
    const sso3rdPartyEnabled = () =>
      Array.isArray(globals.ssoIdentityProviders) &&
      globals.ssoIdentityProviders.length > 0 &&
      ['pro', 'cloud', 'pro-lite'].includes(globals.consoleType);

    const render3rdPartySSOLogin = () => {
      return globals.ssoIdentityProviders.map((idp, i) => (
        <SSOLoginButton
          key={`idp-${idp.client_id}-${i}`}
          location={location}
          clientId={idp.client_id}
          name={idp.name}
          authorizationUrl={idp.authorization_url}
          scope={idp.scope}
        />
      ));
    };

    const handleLoginClick = e => {
      e.preventDefault();
      if (this.state.loginMethod === 'admin-secret') {
        dispatch(loginClicked());
      }
      if (this.state.loginMethod === 'access-token') {
        dispatch(patLoginClicked());
      }
    };

    const handleLoginInProgress = () => {
      if (loginInProgress) {
        return <FaSpinner className={styles.loginLoader} />;
      }
      return (
        <img
          className={styles.loginIcon}
          src={loginIcon}
          alt={'login icon'}
          onClick={handleLoginClick}
        />
      );
    };

    const renderPlaceholder = () => {
      if (this.state.loginMethod === 'admin-secret') {
        return `Enter ${globals.adminSecretLabel}`;
      }
      return `Enter ${globals.patLabel}`;
    };

    const handleInputChange = e => {
      if (this.state.loginMethod === 'admin-secret') {
        this.handleAdminSecret(e);
      }
      if (this.state.loginMethod === 'access-token') {
        this.handlePAT(e);
      }
    };

    const renderInput = () => {
      return (
        <div className={'input-group ' + styles.input_group}>
          <input
            onChange={handleInputChange}
            className={styles.form_input + ' form-control'}
            type="password"
            placeholder={renderPlaceholder()}
            name="password"
            aria-describedby="basic-addon2"
          />
          <span
            className={'input-group-addon ' + styles.input_addon_group}
            id="basic-addon2"
          >
            {handleLoginInProgress()}
          </span>
        </div>
      );
    };

    const renderDropdownOptions = () => {
      return dropdownOptions.map(option => {
        if (option.value === 'access-token' && globals.pro !== true) {
          return null;
        }
        return (
          <option
            key={option.value}
            className={styles.form_input + ' form-control'}
            value={option.value}
          >
            {option.title}
          </option>
        );
      });
    };

    const renderTitle = () => {
      if (globals.consoleMode === 'server') {
        return (
          <div
            className={`${styles.display_flex} ${styles.add_mar_bottom_small} relative z-10`}
          >
            Enter your admin secret
            <IconTooltip message="Admin secret is the secret key to access your GraphQL API in admin mode. If you own this project, you can find the admin secret on the projects dashboard." />
          </div>
        );
      }
      return (
        <div
          className={`input-group ${styles.input_group} ${styles.add_mar_bottom_mid}`}
        >
          <select
            className={styles.form_input + ' form-control'}
            value={this.state.loginMethod}
            onChange={e => {
              this.setState({ loginMethod: e.target.value });
            }}
            placeholder="Select Login Method"
          >
            {renderDropdownOptions()}
          </select>
        </div>
      );
    };

    const doSSOLogin = () => {
      clearAdminSecretState();
      initiateOAuthRequest(location, false);
    };

    const doAdminSecretLogin = () => {
      this.setState({ ...this.state, showAdminSecretLogin: true });
    };

    const backToLoginHome = () => {
      this.setState({ ...this.state, showAdminSecretLogin: false });
    };

    if (globals.consoleType === 'pro-lite' && !globals.ssoEnabled) {
      // the ssoEnabled boolean variable enables Hasura Lux SSO
      // 3rd-party SSO buttons are rendered regardless the ssoEnabled variable is enabled or not
      return (
        <AdminSecretLogin>
          {sso3rdPartyEnabled() && (
            <div className="space-y-md mb-md">{render3rdPartySSOLogin()}</div>
          )}
        </AdminSecretLogin>
      );
    }

    if (globals.consoleType === 'pro-lite' && globals.ssoEnabled) {
      return (
        <div>
          {this.state.showAdminSecretLogin ? (
            <AdminSecretLogin backToLoginHome={backToLoginHome}>
              {sso3rdPartyEnabled() && (
                <div className="space-y-md mb-md">
                  {render3rdPartySSOLogin()}
                </div>
              )}
            </AdminSecretLogin>
          ) : (
            <SSOLogin
              ssoIcon={ssoIcon}
              hasuraEELogo={hasuraEELogo}
              doAdminSecretLogin={doAdminSecretLogin}
              doSSOLogin={doSSOLogin}
              keyIcon={keyIcon}
            >
              {sso3rdPartyEnabled() && render3rdPartySSOLogin()}
            </SSOLogin>
          )}
        </div>
      );
    }

    return (
      <div className={styles.mainWrapper + ' container-fluid'}>
        <div className={styles.container + ' container'} id="login">
          <div className={styles.loginCenter}>
            <Helmet title={'Login | ' + 'Hasura'} />
            <div className={styles.hasuraLogo}>
              <img src={hasuraLogo} alt="hasura logo" />
            </div>
            <div className={styles.loginWrapper}>
              <form className="form-horizontal" onSubmit={handleLoginClick}>
                {sso3rdPartyEnabled() && (
                  <div className="px-md mt-sm space-y-md">
                    {render3rdPartySSOLogin()}
                  </div>
                )}
                <div
                  className={styles.input_addon_group + ' ' + styles.padd_top}
                >
                  {renderTitle()}
                  {renderInput()}
                </div>
                {renderLogin()}
              </form>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

Login.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const generatedLoginConnector = conn => {
  const mapStateToProps = (state, ownProps) => {
    return {
      location: ownProps.location,
      loginInProgress: state.main.loginInProgress,
      adminSecretError: state.tables.adminSecretError,
      featuresCompatibility: state.main.featuresCompatibility,
    };
  };
  return conn(mapStateToProps)(Login);
};

export default generatedLoginConnector;
