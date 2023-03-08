import React, { useEffect } from 'react';
import { GraphQLError } from 'graphql';
import { SimpleForm } from '../../../../../new-components/Form';
import { Dialog } from '../../../../../new-components/Dialog';
import { useFireNotification } from '../../../../../new-components/Notifications';
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
        message: 'Error fetching Environment Variables',
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
    if (successCb) successCb();
  };

  const updateTenantEnvErrorCb = (error?: GraphQLError) => {
    setFormState('error');
    fireNotification({
      type: 'error',
      title: 'Error!',
      message: 'Error updating Environment Variables',
      error,
    });
  };

  const { onSubmitHandler: onUpdateTenantEnvSubmitHandler } =
    useUpdateTenantEnv(updateTenantEnvSuccessCb, updateTenantEnvErrorCb);

  const onSubmit = (formData: Record<string, unknown>) => {
    setFormState('loading');

    // process form data before setting as environment vars
    Object.entries(formData).forEach(([k, v]) => {
      formData[k] = (v as string).trim();

      const envVar = envVars.find(env => k === env.Name);
      if (envVar?.ValueType === 'STRING_ARRAY') {
        formData[k] = JSON.stringify(
          (v as string).split(',').map(d => d.trim())
        );
      }
    });

    const updateTenantEnvInput: UpdateEnvObj[] = [];
    Object.entries(formData).forEach(([key, value]) => {
      updateTenantEnvInput.push({ key, value: value as string });
    });

    onUpdateTenantEnvSubmitHandler(tenantHash as string, updateTenantEnvInput);
  };

  return (
    <Dialog size="xl" hasBackdrop>
      <SimpleForm
        schema={schema}
        onSubmit={onSubmit}
        options={{ defaultValues }}
      >
        <div className="max-h-[calc(100vh-20rem)] overflow-y-auto font-sans px-8 pt-10 mb-sm">
          <div className="text-4xl text-slate-900 font-semibold mb-xs">
            Environment Variables
          </div>
          <div className="text-gray-600 font-normal text-m mb-md">
            The following variables are required to set up your project.
          </div>

          <EnvVarsFormFields envVars={envVars} />
        </div>
        <CustomFooter formState={formState} />
      </SimpleForm>
    </Dialog>
  );
}
