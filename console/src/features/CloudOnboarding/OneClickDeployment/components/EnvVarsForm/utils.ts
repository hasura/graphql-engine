import { z, ZodString } from 'zod';
import { NeonButtonProps } from './types';
import { RequiredEnvVar } from '../../types';
import { NeonIntegrationStatus } from '../../../../../components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import { getProjectHealth } from '../../../../../components/Services/Data/DataSources/CreateDataSource/utils';

export const generateDefaultValues = (envVars: RequiredEnvVar[]) => {
  const defaultValues: any = {};
  envVars.forEach(envVar => {
    if (envVar.Default) defaultValues[envVar.Name] = String(envVar.Default);
  });
  return defaultValues;
};

export const generateEnvVarsFormSchema = (envVars: RequiredEnvVar[]) => {
  const dynamicSchemaObject: Record<string, ZodString> = {};

  envVars.forEach(envVar => {
    if (envVar.Mandatory) {
      dynamicSchemaObject[envVar.Name] = z
        .string()
        .min(1, `${envVar.Name} is a required field!`);
    } else {
      dynamicSchemaObject[envVar.Name] = z.string();
    }
  });

  return z.object(dynamicSchemaObject);
};

export const getFormProperties = (
  envVars: RequiredEnvVar[],
  tenantEnvVars: Record<string, string>
) => {
  const schema = generateEnvVarsFormSchema(envVars);
  const defaultValues = generateDefaultValues(envVars);

  for (const key of Object.keys(defaultValues)) {
    if (!defaultValues[key] && tenantEnvVars?.[key]) {
      defaultValues[key] = tenantEnvVars?.[key];
    }
  }

  return { schema, defaultValues };
};

export const transformNeonIntegrationStatusToNeonButtonProps = (
  neonIntegrationStatus: NeonIntegrationStatus
): NeonButtonProps => {
  let neonButtonProps: NeonButtonProps;
  switch (neonIntegrationStatus.status) {
    case 'idle':
      neonButtonProps = {
        status: {
          status: 'default',
        },
        buttonText: 'Create New Database',
        onClickConnect: neonIntegrationStatus.action,
        icon: 'create',
      };
      break;
    case 'authentication-loading':
      neonButtonProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Authenticating with Neon',
        onClickConnect: () => null,
        icon: 'loading',
      };
      break;
    case 'authentication-error':
      neonButtonProps = {
        status: {
          status: 'error',
          errorTitle: neonIntegrationStatus.title,
          errorDescription: neonIntegrationStatus.description,
        },
        buttonText: 'Try again',
        onClickConnect: neonIntegrationStatus.action,
        icon: 'refresh',
      };
      break;
    case 'authentication-success':
    case 'neon-database-creation-loading':
      neonButtonProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Creating Database',
        onClickConnect: () => null,
        icon: 'loading',
      };
      break;
    case 'neon-database-creation-error':
      neonButtonProps = {
        status: {
          status: 'error',
          errorTitle: neonIntegrationStatus.title,
          errorDescription: neonIntegrationStatus.description,
        },
        buttonText: 'Retry Creating Database',
        onClickConnect: neonIntegrationStatus.action,
        icon: 'refresh',
      };
      break;
    case 'neon-database-creation-success':
      neonButtonProps = {
        status: {
          status: 'success',
        },
        buttonText: '',
        onClickConnect: () => null,
        dbURL: neonIntegrationStatus.payload?.databaseUrl,
      };
      break;
    default:
      neonButtonProps = {
        status: {
          status: 'default',
        },
        buttonText: 'Create New Database',
        onClickConnect: () => null,
        icon: 'create',
      };
      break;
  }
  return neonButtonProps;
};

export const verifyProjectHealthAndProceed = (
  successCallback: VoidFunction,
  errorCallback: VoidFunction,
  retryCount = 0
) => {
  if (retryCount === 10) {
    errorCallback();
    return;
  }
  getProjectHealth()
    .then(() => {
      successCallback();
    })
    .catch(() => {
      setTimeout(() => {
        verifyProjectHealthAndProceed(
          successCallback,
          errorCallback,
          retryCount + 1
        );
      }, 1500);
    });
};
