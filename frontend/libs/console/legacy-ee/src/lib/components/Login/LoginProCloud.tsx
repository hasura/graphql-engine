import { useState } from 'react';
import useNavigateAuth from '../../shared/auth/useNavigateAuth';
import type { EnterpriseAuthState } from '../../shared/auth/types';
import { ADMIN_SECRET_HEADER_KEY } from '@hasura/shared/types';
import useEnterpriseAuth from '../../shared/auth/useEnterpriseAuth';
import {
  Checkbox,
  CheckedState,
  FieldLabel,
  IconButton,
  Input,
  Select,
  Spinner,
} from '@hasura/shared/ui';
import { CiLogin } from 'react-icons/ci';
import { useAppContext } from '@hasura/shared/context';

const dropdownOptions = [
  { label: 'Admin Secret', value: 'admin-secret' },
  { label: 'Personal Access Token', value: 'access-token' },
];

const LoginProCloud = () => {
  const { authenticate } = useEnterpriseAuth();
  const { envVars } = useAppContext();
  const navigateAuth = useNavigateAuth();
  const [loginMethod, setLoginMethod] = useState<
    'access-token' | 'admin-secret'
  >(envVars.consoleMode === 'cli' ? 'access-token' : 'admin-secret');
  const [password, setPassword] = useState('');
  const [savePassword, setSavePassword] = useState<CheckedState>(false);
  const [loginInProgress, setLoginInProgress] = useState(false);

  const handleLoginClick = (e) => {
    e.preventDefault();
    const input: EnterpriseAuthState =
      loginMethod === 'admin-secret'
        ? {
            type: 'admin-secret',
            adminSecret: password,
            shouldPersist: savePassword === true,
          }
        : {
            type: 'pat',
            pat: password,
          };

    setLoginInProgress(true);
    authenticate(input)
      .then((ok) => {
        if (ok) {
          navigateAuth(input);
        }
      })
      .catch(() => {})
      .finally(() => setLoginInProgress(false));
  };

  const handleLoginInProgress = () => {
    if (loginInProgress) {
      return <Spinner />;
    }

    return (
      <IconButton mode="default" onClick={handleLoginClick} icon={CiLogin} />
    );
  };

  const renderPlaceholder = () => {
    if (loginMethod === 'admin-secret') {
      return `Enter ${ADMIN_SECRET_HEADER_KEY}`;
    }

    return `Enter personal access token`;
  };

  const handleInputChange: React.ChangeEventHandler<HTMLInputElement> = (e) => {
    setPassword(e.target.value);
  };

  const renderInput = () => {
    return (
      <div className="mt-4">
        <Input
          onChange={handleInputChange}
          type="password"
          placeholder={renderPlaceholder()}
          name="password"
          aria-describedby="basic-addon2"
          rightButton={handleLoginInProgress()}
        />

        <div className="mt-4">
          <Checkbox value={savePassword} onChange={setSavePassword}>
            Remember on the browser
          </Checkbox>
        </div>
      </div>
    );
  };

  const renderTitle = () => {
    if (envVars.consoleMode === 'server') {
      return (
        <FieldLabel
          label="Enter your admin secret"
          tooltip="Admin secret is the secret key to access your GraphQL API in admin mode. If you own this project, you can find the admin secret on the projects dashboard."
        />
      );
    }

    return (
      <div>
        <Select
          value={loginMethod}
          onChange={(value) => {
            setLoginMethod(value as 'admin-secret' | 'access-token');
          }}
          placeholder="Select Login Method"
          options={dropdownOptions}
        />
      </div>
    );
  };

  return (
    <form onSubmit={handleLoginClick}>
      <div className="mb-4">
        {renderTitle()}
        {renderInput()}
      </div>
    </form>
  );
};

export default LoginProCloud;
