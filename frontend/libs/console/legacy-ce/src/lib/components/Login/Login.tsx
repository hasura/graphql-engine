import React, { useState } from 'react';
import { Connect } from 'react-redux';
import { Analytics, REDACT_EVERYTHING } from '../../features/Analytics';
import { Button } from '../../new-components/Button';
import {
  CheckboxesField,
  InputField,
  SimpleForm,
} from '../../new-components/Form';
import { push } from 'react-router-redux';
import { z } from 'zod';
import Helmet from 'react-helmet';
import { FaSpinner } from 'react-icons/fa';
import globals from '../../Globals';
import { verifyLogin } from './Actions';
import { CLI_CONSOLE_MODE } from '../../constants';
import { getAdminSecret } from '../Services/ApiExplorer/ApiRequest/utils';
import { ConnectInjectedProps } from '../../types';
import { isProConsole } from '../../utils/proConsole';

import hasuraLogo from './black-logo.svg';
import hasuraEELogo from './black-logo-ee.svg';

const validationSchema = z.object({
  password: z.string().min(1, { message: 'Please add password' }),
  savePassword: z.enum(['checked']).array().optional(),
});

const Login: React.FC<ConnectInjectedProps> = ({ dispatch, children }) => {
  // request state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  const getLoginForm = () => {
    const getLoginButtonText = () => {
      // login button text
      let loginText: React.ReactNode = 'Enter';
      if (loading) {
        loginText = (
          <span>
            Verifying...
            <FaSpinner className="animate-spin" />
          </span>
        );
      } else if (error) {
        loginText = 'Error. Try again?';
      }

      return loginText;
    };

    // form submit handler
    const onSubmit = ({ password, savePassword }: any) => {
      const successCallback = () => {
        setLoading(false);
        setError(null);
        dispatch(push(globals.urlPrefix));
      };

      const errorCallback = (err: Error) => {
        setLoading(false);
        setError(err);
      };

      setLoading(true);

      verifyLogin({
        adminSecret: password,
        shouldPersist: Boolean(savePassword),
        successCallback,
        errorCallback,
        dispatch,
      });
    };

    return (
      <SimpleForm
        schema={validationSchema}
        options={{
          defaultValues: undefined,
        }}
        onSubmit={onSubmit}
      >
        <Analytics name="Login" {...REDACT_EVERYTHING}>
          <div className="flex flex-col bg-white p-4">
            {!!children && <div>{children}</div>}
            <div>
              <div className="w-full">
                <InputField
                  name="password"
                  type="password"
                  size="full"
                  placeholder="Enter admin-secret"
                />
              </div>
            </div>
            <div className="w-full">
              <Button
                full
                type="submit"
                mode="primary"
                size="md"
                disabled={loading}
              >
                {getLoginButtonText()}
              </Button>
            </div>
            <div>
              <label className="cursor-pointer flex items-center pt-sm">
                <CheckboxesField
                  name="savePassword"
                  options={[
                    {
                      value: 'checked',
                      label: 'Remember on the browser',
                    },
                  ]}
                />
              </label>
            </div>
          </div>
        </Analytics>
      </SimpleForm>
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
      <div className="text-center">
        {adminSecret ? invalidAdminSecretMessage : missingAdminSecretMessage}
      </div>
    );
  };

  const showLogo = isProConsole(globals) ? (
    <img className="flex w-36 mx-auto" src={hasuraEELogo} alt="Hasura EE" />
  ) : (
    <img src={hasuraLogo} alt="Hasura" />
  );

  return (
    <div className="flex justify-center items-center min-h-screen bg-gray-100">
      <div className="flex" id="login">
        <div className="">
          <Helmet title="Login | Hasura" />
          <div className="flex justify-center mb-md">{showLogo}</div>
          <div className="w-[400px] border shadow-lg p-md rounded-lg bg-white">
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
