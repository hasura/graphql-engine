import React, { useState } from 'react';
import { Code, Flex } from '@radix-ui/themes';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  Button,
  CheckboxField,
  InputField,
  SimpleForm,
  Text,
} from '@hasura/shared/ui';
import { z } from 'zod';
import { CLI_CONSOLE_MODE } from '@hasura/shared/types';
import { useNavigate } from 'react-router';
import { useAppContext, useAuthContext } from '@hasura/shared/context';
import AdminSecretDisabledMessage from './AdminSecretDisabledMessage';

const validationSchema = z.object({
  password: z.string().min(1, { message: 'Please add password' }),
  savePassword: z.boolean().nullish(),
});

type LoginFormValues = z.infer<typeof validationSchema>;

type LoginProps = {
  children?: React.ReactNode;
};

const AdminSecretLoginForm: React.FC<LoginProps> = ({ children }) => {
  const navigate = useNavigate();
  const { envVars } = useAppContext();
  const { authenticate } = useAuthContext();
  // request state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  const getLoginForm = () => {
    const getLoginButtonText = () => {
      // login button text
      let loginText: React.ReactNode = 'Enter';
      if (error) {
        loginText = 'Error. Try again?';
      }

      return loginText;
    };

    // form submit handler
    const onSubmit = ({ password, savePassword }: LoginFormValues) => {
      const successCallback = () => {
        setLoading(false);
        setError(null);
        navigate('/');
      };

      const errorCallback = (err: Error) => {
        setLoading(false);
        setError(err);
      };

      setLoading(true);

      authenticate({
        type: 'admin-secret',
        adminSecret: password,
        shouldPersist: Boolean(savePassword),
      })
        .then(successCallback)
        .catch(errorCallback);
    };

    return (
      <SimpleForm schema={validationSchema} onSubmit={onSubmit}>
        <Analytics name="Login" {...REDACT_EVERYTHING}>
          <Flex direction="column" gap="2">
            {children && <div>{children}</div>}
            <InputField
              name="password"
              fieldProps={{
                type: 'password',
                placeholder: 'Enter admin-secret',
              }}
              size="full"
              noErrorPlaceholder
            />
            <Button
              className="w-full"
              type="submit"
              mode="primary"
              size="md"
              loading={loading}
              loadingText="Verifying..."
            >
              {getLoginButtonText()}
            </Button>
            <div className="mt-2">
              <CheckboxField name="savePassword" noErrorPlaceholder>
                Remember on the browser
              </CheckboxField>
            </div>
          </Flex>
        </Analytics>
      </SimpleForm>
    );
  };

  const getCLIAdminSecretErrorMessage = () => {
    const adminSecret = envVars.adminSecret;

    return (
      <Text as="p" align="center">
        {adminSecret ? (
          <span>Invalid admin-secret passed from CLI</span>
        ) : (
          <span>
            Seems like your Hasura GraphQL engine instance has an admin-secret
            configured.
            <br />
            Run console with the admin-secret using:
            <br />
            <br />
            <Code>hasura console --admin-secret=&lt;your-admin-secret&gt;</Code>
          </span>
        )}
      </Text>
    );
  };

  return (
    <>
      {envVars.consoleMode !== CLI_CONSOLE_MODE ? (
        envVars.isAdminSecretDisabled ? (
          <AdminSecretDisabledMessage />
        ) : (
          getLoginForm()
        )
      ) : (
        getCLIAdminSecretErrorMessage()
      )}
    </>
  );
};

export default AdminSecretLoginForm;
