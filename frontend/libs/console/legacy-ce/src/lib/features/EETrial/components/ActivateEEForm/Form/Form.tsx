import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import {
  CheckboxesField,
  InputField,
  Button,
  SelectField,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

import { Analytics } from '@hasura/shared/analytics';
import { ConsentCheckbox } from './ConsentCheckbox';
import {
  ActivateEEFormSchema,
  activationSchema,
  RegisterEEFormSchema,
  registrationSchema,
} from './schema';
import { useRegisterEETrial } from './useRegisterEETrial';
import { useActivateEETrial } from './useActivateEETrial';
import { EE_TRIAL_DOCS_URL } from '../../../constants';

type FormState = 'register' | 'activate';
type Props = {
  onSuccess?: VoidFunction;
  formState?: FormState;
};

export const Form: React.FC<Props> = (props) => {
  const { onSuccess } = props;
  const [state, setState] = React.useState<FormState>(
    props.formState || 'register',
  );

  const onActivation = () => {
    if (onSuccess) {
      onSuccess();
    }
  };

  return (
    <Flex direction="column" className="w-full">
      <div className="p-4">
        <div>
          {state === 'register' && (
            <Flex direction="column" className="w-full">
              <RegistrationForm {...props} onSuccess={onActivation} />
              <Flex justify="center" className="w-full mt-2 text-sm">
                <span className="mr-1">Already registered?</span>
                <Analytics name="ee-activate-existing-license">
                  <a
                    className="text-secondary"
                    role="button"
                    onClick={() => {
                      setState('activate');
                    }}
                  >
                    {' '}
                    Activate Existing License{' '}
                  </a>
                </Analytics>
              </Flex>
            </Flex>
          )}
          {state === 'activate' && (
            <Flex direction="column" className="w-full">
              <ActivationForm {...props} onSuccess={onActivation} />
              <Flex justify="center" className="w-full mt-2 text-sm">
                <span className="mr-1">Do not have a license?</span>
                <Analytics name="ee-register-a-new-license">
                  <a
                    className="text-secondary"
                    role="button"
                    onClick={() => {
                      setState('register');
                    }}
                  >
                    Register for Hasura Enterprise Trial
                  </a>
                </Analytics>
              </Flex>
            </Flex>
          )}
        </div>
      </div>
    </Flex>
  );
};

export const ActivationForm: React.FC<Props> = (props: Props) => {
  const { onSuccess } = props;

  const { activateEETrial, isLoading, errorMessage } =
    useActivateEETrial(onSuccess);

  const onSubmit = (data: ActivateEEFormSchema) => {
    activateEETrial(data);
  };

  const methods = useForm<ActivateEEFormSchema>({
    resolver: zodResolver(activationSchema),
  });

  const handleSubmitClick = () => {
    methods.handleSubmit(onSubmit)();
  };

  return (
    <FormProvider {...methods}>
      <form className="space-y-2">
        <h1 className="text-xl text-slate-900 font-semibold mb-1">
          Activate your free Hasura Enterprise trial license
        </h1>
        <div className="text-muted mt-0 mb-1">
          Unlock extra observability, security, and performance features for
          your Hasura instance.
        </div>
        <Analytics name="ee-activation-form-email" passHtmlAttributesToChildren>
          <InputField
            name="email"
            label="Email *"
            description="Work email preferred"
            fieldProps={{ placeholder: 'name@work.com' }}
            noErrorPlaceholder
          />
        </Analytics>
        <Analytics
          name="ee-activation-form-password"
          passHtmlAttributesToChildren
        >
          <InputField
            name="password"
            label="Password *"
            fieldProps={{ type: 'password', placeholder: 'Password' }}
          />
        </Analytics>
        {errorMessage ? (
          <div className="font-semibold text-red-600 bg-red-50 p-3 rounded w-full">
            {errorMessage}
          </div>
        ) : null}
        <Flex direction="column" gap="4">
          <Analytics
            name="ee-activation-form-submit"
            passHtmlAttributesToChildren
          >
            <Button
              type="button"
              mode="primary"
              onClick={handleSubmitClick}
              loading={isLoading}
              loadingText="Activating..."
              className="w-full"
            >
              Activate Your Trial License
            </Button>
          </Analytics>
        </Flex>
      </form>
    </FormProvider>
  );
};

export const RegistrationForm: React.FC<Props> = (props: Props) => {
  const { onSuccess } = props;

  const { registerEETrial, isLoading, errorMessage } =
    useRegisterEETrial(onSuccess);

  const onSubmit = (data: RegisterEEFormSchema) => {
    registerEETrial(data);
  };

  const methods = useForm<RegisterEEFormSchema>({
    resolver: zodResolver(registrationSchema),
    defaultValues: { consent: false },
  });

  const handleSubmitClick = () => {
    methods.handleSubmit(onSubmit)();
  };

  const [hasuraUseCaseOptions] = React.useState(() =>
    [
      { value: 'data-api', label: 'Data API on my databases' },
      {
        value: 'data-federation',
        label: 'Data Federation across APIs and databases',
      },
      { value: 'gql-backend', label: 'GraphQL Backend' },
      { value: 'api-gateway', label: 'API Gateway' },
    ].sort(() => Math.random() - 0.5),
  );

  return (
    <FormProvider {...methods}>
      <form className="space-y-2">
        <h1 className="text-xl text-slate-900 font-semibold mb-1">
          Activate your free Hasura Enterprise trial license
        </h1>
        <div className="text-muted mt-0 mb-1">
          Unlock extra observability, security, and performance features for
          your Hasura instance.&nbsp;
          <Analytics name="ee-trial-docs">
            <a
              href={EE_TRIAL_DOCS_URL}
              target="_blank"
              rel="noopener noreferrer"
            >
              Read more
            </a>
            .
          </Analytics>
        </div>
        <Flex gap="4">
          <Analytics
            name="ee-registration-form-first-name"
            passHtmlAttributesToChildren
          >
            <InputField
              name="firstName"
              label="First Name *"
              fieldProps={{ placeholder: 'First Name...' }}
              noErrorPlaceholder
            />
          </Analytics>
          <Analytics
            name="ee-registration-form-last-name"
            passHtmlAttributesToChildren
          >
            <InputField
              name="lastName"
              label="Last Name *"
              fieldProps={{ placeholder: 'Last Name...' }}
              noErrorPlaceholder
            />
          </Analytics>
        </Flex>
        <Analytics
          name="ee-registration-form-email"
          passHtmlAttributesToChildren
        >
          <InputField
            name="email"
            label="Email *"
            description="Work email preferred"
            tooltip="If you already have a Hasura Cloud account, please use the same email and password for this registration"
            fieldProps={{ placeholder: 'name@work.com' }}
            noErrorPlaceholder
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-password"
          passHtmlAttributesToChildren
        >
          <InputField
            name="password"
            label="Password *"
            fieldProps={{ type: 'password', placeholder: 'Password' }}
            noErrorPlaceholder
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-organization"
          passHtmlAttributesToChildren
        >
          <InputField
            name="organization"
            label="Organization *"
            fieldProps={{ placeholder: 'My Work Inc.' }}
            noErrorPlaceholder
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-position"
          passHtmlAttributesToChildren
        >
          <InputField
            name="jobFunction"
            label="Position"
            fieldProps={{ placeholder: 'Software Developer' }}
            noErrorPlaceholder
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-phone"
          passHtmlAttributesToChildren
        >
          <InputField
            name="phoneNumber"
            label="Phone Number"
            fieldProps={{ placeholder: '+1 123-345-6789' }}
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-enterprise-use-case"
          passHtmlAttributesToChildren
        >
          <CheckboxesField
            name="eeUseCase"
            label="What brings you to Hasura Enterprise? *"
            options={[
              {
                value: 'ee-db-support',
                label:
                  'Enterprise only database support (MySQL, Oracle, Snowflake etc.)',
              },
              {
                value: 'ee-features',
                label:
                  'Enterprise-only features (Observability, Security, Performance etc.)',
              },
            ]}
            orientation="horizontal"
          />
        </Analytics>
        <Analytics
          name="ee-registration-form-hasura-use-case"
          passHtmlAttributesToChildren
        >
          <SelectField
            name="hasuraUseCase"
            options={hasuraUseCaseOptions}
            label="What would you like to Build with Hasura? *"
            placeholder="Please select"
            fieldProps={{
              trigger: {
                className: 'w-full!',
              },
            }}
          />
        </Analytics>
        <Analytics name="ee-registration-form-tos-consent">
          <ConsentCheckbox fieldName="consent" />
        </Analytics>
        {errorMessage ? (
          <div className="font-semibold text-red-600 bg-red-50 p-3 rounded w-full">
            {errorMessage}
          </div>
        ) : null}
        <Flex direction="column" gap="4" className="mt-4">
          <Analytics
            name="ee-registration-form-submit"
            passHtmlAttributesToChildren
          >
            <Button
              type="button"
              mode="primary"
              onClick={handleSubmitClick}
              loading={isLoading}
              loadingText="Activating..."
              className="w-full"
            >
              Activate Your Trial License
            </Button>
          </Analytics>
        </Flex>
      </form>
    </FormProvider>
  );
};
