import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { EnvVarsForm, EnvVarsFormProps } from './EnvVarsForm';
import { RequiredEnvVar } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Env Vars Form',
  component: EnvVarsForm,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof EnvVarsForm>;

const sampleEnvVars: RequiredEnvVar[] = [
  {
    Kind: 'ENV_TYPE_STATIC',
    Name: 'STREAMING_SUBSCRIPTION_FLAG',
    Default: 'true',
    SubKind: '',
    Mandatory: false,
    Description: '',
  },
  {
    // Default null
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'PG_DATABASE_URL_1',
    SubKind: 'postgres',
    Mandatory: false,
    Description: 'Postgres database URL 1',
  },
  {
    // Default null
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'PG_DATABASE_URL_2',
    SubKind: 'postgres',
    Mandatory: false,
    Description: 'Postgres database URL 2',
  },
  {
    // Default null
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'MYSQL_DATABASE_URL',
    SubKind: 'mysql',
    Mandatory: true,
    Description: 'MYSQL database URL',
  },
  {
    Kind: 'ENV_TYPE_DYNAMIC',
    Name: 'ACTION_WEBHOOK_URL',
    Default: 'http://localhost:3000',
    SubKind: '',
    Mandatory: true,
    Description: 'Action webhook url',
  },
];

const sampleEnvVarsMinimal: RequiredEnvVar[] = [
  {
    // Default null
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'PG_DATABASE_URL_1',
    SubKind: 'postgres',
    Mandatory: false,
    Description: 'Postgres database URL 1',
  },
  {
    Kind: 'ENV_TYPE_DYNAMIC',
    Name: 'ACTION_WEBHOOK_URL',
    Default: 'http://localhost:3000',
    SubKind: '',
    Mandatory: true,
    Description: 'Action webhook url',
  },
];

export const Default: Story<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVars}
    formState="default"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);

export const Loading: Story<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVarsMinimal}
    formState="loading"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);

export const Error: Story<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVarsMinimal}
    formState="error"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);

export const Success: Story<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVarsMinimal}
    formState="success"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);
