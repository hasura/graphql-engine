import React, { useEffect } from 'react';
import { GraphQLError } from 'graphql';
import { SimpleForm } from '@/new-components/Form';
import { Dialog } from '@/new-components/Dialog';
import { useFireNotification } from '@/new-components/Notifications';
import { useGetTenantEnvs, useUpdateTenantEnv } from './hooks';
import { getFormProperties } from './utils';
import { EnvVarsFormState, RequiredEnvVar } from '../../types';
import { UpdateEnvObj } from './types';
import { EnvVarsFormFields, CustomFooter } from './components';

export type EnvVarsFormProps = {
  envVars: RequiredEnvVar[];
  formState: EnvVarsFormState;
  setFormState: React.Dispatch<React.SetStateAction<EnvVarsFormState>>;
  successCb?: () => void;
  errorCb?: () => void;
};

export function EnvVarsForm(props: EnvVarsFormProps) {
  const { fireNotification } = useFireNotification();
  const { envVars, formState, setFormState, successCb } = props;

  const { data: tenantEnvData } = useGetTenantEnvs();

  useEffect(() => {
    if (
      tenantEnvData &&
      tenantEnvData.errors &&
      tenantEnvData.errors.length > 0
    ) {
      fireNotification({
        type: 'error',
        title: 'Error!',
        message: 'Error fetching environment variables',
        error: tenantEnvData.errors[0],
      });
    }
  }, [tenantEnvData]);

  const tenantHash = tenantEnvData?.data?.getTenantEnv?.hash;
  const tenantEnvVars = tenantEnvData?.data?.getTenantEnv?.envVars;

  const { schema, defaultValues } = React.useMemo(
    () => getFormProperties(envVars, tenantEnvVars),
    [envVars, tenantEnvVars]
  );

  const updateTenantEnvSuccessCb = () => {
    setFormState('success');
    if (successCb) successCb();
  };

  const updateTenantEnvErrorCb = (error?: GraphQLError) => {
    setFormState('error');
    fireNotification({
      type: 'error',
      title: 'Error!',
      message: 'Error updating environment variables',
      error,
    });
  };

  const { onSubmitHandler: onUpdateTenantEnvSubmitHandler } =
    useUpdateTenantEnv(updateTenantEnvSuccessCb, updateTenantEnvErrorCb);

  const onSubmit = (formData: Record<string, unknown>) => {
    setFormState('loading');

    Object.keys(formData).forEach(k => {
      formData[k] = (formData[k] as string).trim();
    });

    const updateTenantEnvInput: UpdateEnvObj[] = [];
    Object.entries(formData).forEach(([key, value]) => {
      updateTenantEnvInput.push({ key, value: value as string });
    });

    onUpdateTenantEnvSubmitHandler(tenantHash as string, updateTenantEnvInput);
  };

  return (
    <Dialog title="Environment Variables" size="xl" hasBackdrop>
      <SimpleForm
        schema={schema}
        onSubmit={onSubmit}
        options={{ defaultValues }}
      >
        <div className="px-4">
          <div className="text-gray-600 mb-6">
            Environment variables are required to complete loading your project.
          </div>

          <EnvVarsFormFields envVars={envVars} />
        </div>
        <CustomFooter formState={formState} />
      </SimpleForm>
    </Dialog>
  );
}
