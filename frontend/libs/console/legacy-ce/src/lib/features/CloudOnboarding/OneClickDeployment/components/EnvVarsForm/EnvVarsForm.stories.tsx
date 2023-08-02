import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { EnvVarsForm, EnvVarsFormProps } from './EnvVarsForm';
import { RequiredEnvVar } from '../../types';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Env Vars Form',
  component: EnvVarsForm,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof EnvVarsForm>;

const sampleEnvVars: RequiredEnvVar[] = [
  {
    Kind: 'ENV_TYPE_STATIC',
    Name: 'STREAMING_SUBSCRIPTION_FLAG',
    Default: 'true',
    Mandatory: false,
    Description: '',
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'PG_DATABASE_URL_1',
    SubKind: 'postgres',
    Mandatory: false,
    Description: 'Postgres database URL 1',
    Position: 1,
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'PG_DATABASE_URL_2',
    SubKind: 'postgres',
    Mandatory: false,
    Description: 'Postgres database URL 2',
    Position: 1,
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'MYSQL_DATABASE_URL',
    SubKind: 'mysql',
    Mandatory: true,
    Description: 'MYSQL database URL',
    Position: 2,
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'BIGQUERY_SERVICE_ACCOUNT',
    SubKind: 'bigquery',
    Mandatory: true,
    ValueType: 'JSON',
    Description: 'Service account key',
    Position: 4,
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'BIGQUERY_PROJECT_ID',
    SubKind: 'bigquery',
    Mandatory: true,
    Description: 'Bigquery Project Id',
    Position: 4,
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'BIGQUERY_DATASETS',
    SubKind: 'bigquery',
    Mandatory: true,
    Description: 'Big query datasets',
    ValueType: 'STRING_ARRAY',
    Position: 4,
    Placeholder: 'dataset1, dataset2',
  },
  {
    Kind: 'ENV_TYPE_DATABASE',
    Name: 'BIGQUERY_SERVICE_ACCOUNT_2',
    SubKind: 'bigquery',
    Mandatory: true,
    ValueType: 'JSON',
    Description: 'Service account key 2',
    Position: 5,
  },
  {
    Kind: 'ENV_TYPE_DYNAMIC',
    Name: 'ACTION_WEBHOOK_URL',
    Default: 'http://localhost:3000',
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
    Mandatory: true,
    Description: 'Action webhook url',
  },
];

export const Default: StoryFn<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVars}
    formState="default"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);

export const Loading: StoryFn<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVarsMinimal}
    formState="loading"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);

export const Error: StoryFn<EnvVarsFormProps> = () => (
  <EnvVarsForm
    envVars={sampleEnvVarsMinimal}
    formState="error"
    setFormState={() => {}}
    successCb={() => {}}
    errorCb={() => {}}
  />
);
