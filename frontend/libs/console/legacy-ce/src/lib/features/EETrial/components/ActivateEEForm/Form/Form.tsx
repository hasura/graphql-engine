import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import {
  SelectField,
  CheckboxesField,
  InputField,
} from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import { Analytics } from '../../../../Analytics';
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

export const Form: React.VFC<Props> = props => {
  const { onSuccess } = props;
  const [state, setState] = React.useState<FormState>(
    props.formState || 'register'
  );

  const onActivation = () => {
    if (onSuccess) {
      onSuccess();
    }
  };

  return (
    <div className="flex flex-col w-full">
      <div className="p-md">
        <div>
          {state === 'register' && (
            <div className="flex flex-col w-full">
              <RegistrationForm {...props} onSuccess={onActivation} />
              <div className="flex w-full justify-center mt-xs text-sm">
                <span className="mr-xs">Already registered?</span>
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
              </div>
            </div>
          )}
          {state === 'activate' && (
            <div className="flex flex-col w-full">
              <ActivationForm {...props} onSuccess={onActivation} />
              <div className="flex w-full justify-center mt-xs text-sm">
                <span className="mr-xs">Do not have a license?</span>
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
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
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
        <h1 className="text-xl text-slate-900 font-semibold mb-xs">
          Activate your free Hasura Enterprise trial license
        </h1>
        <div className="text-muted mt-0 mb-xs">
          Unlock extra observability, security, and performance features for
          your Hasura instance.
        </div>
        <Analytics name="ee-activation-form-email" passHtmlAttributesToChildren>
          <InputField
            name="email"
            label="Email *"
            description="Work email preferred"
            placeholder="name@work.com"
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
            type="password"
            placeholder="Password"
          />
        </Analytics>
        {errorMessage ? (
          <div className="font-semibold text-red-600 bg-red-50 p-3 rounded w-full">
            {errorMessage}
          </div>
        ) : null}
        <div className="flex flex-col gap-4">
          <Analytics
            name="ee-activation-form-submit"
            passHtmlAttributesToChildren
          >
            <Button
              type="button"
              mode="primary"
              onClick={handleSubmitClick}
              isLoading={isLoading}
              loadingText="Activating..."
              full
            >
              Activate Your Trial License
            </Button>
          </Analytics>
        </div>
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

  return (
    <FormProvider {...methods}>
      <form className="space-y-2">
        <h1 className="text-xl text-slate-900 font-semibold mb-xs">
          Activate your free Hasura Enterprise trial license
        </h1>
        <div className="text-muted mt-0 mb-xs">
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
        <div className="flex gap-4">
          <Analytics
            name="ee-registration-form-first-name"
            passHtmlAttributesToChildren
          >
            <InputField
              name="firstName"
              label="First Name *"
              placeholder="First Name..."
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
              placeholder="Last Name..."
              noErrorPlaceholder
            />
          </Analytics>
        </div>
        <Analytics
          name="ee-registration-form-email"
          passHtmlAttributesToChildren
        >
          <InputField
            name="email"
            label="Email *"
            description="Work email preferred"
            tooltip="If you already have a Hasura Cloud account, please use the same email and password for this registration"
            placeholder="name@work.com"
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
            type="password"
            placeholder="Password"
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
            placeholder="My Work Inc."
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
            placeholder="Software Developer"
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
            placeholder="+1 123-345-6789"
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
            options={[
              { value: 'data-api', label: 'Data API on my databases' },
              {
                value: 'data-federation',
                label: 'Data Federation across APIs and databases',
              },
              { value: 'gql-backend', label: 'GraphQL Backend' },
              { value: 'api-gateway', label: 'API Gateway' },
            ].sort(() => Math.random() - 0.5)}
            label="What would you like to Build with Hasura? *"
            placeholder="Please select"
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
        <div className="flex flex-col gap-4">
          <Analytics
            name="ee-registration-form-submit"
            passHtmlAttributesToChildren
          >
            <Button
              type="button"
              mode="primary"
              onClick={handleSubmitClick}
              isLoading={isLoading}
              loadingText="Activating..."
              full
            >
              Activate Your Trial License
            </Button>
          </Analytics>
        </div>
      </form>
    </FormProvider>
  );
};
