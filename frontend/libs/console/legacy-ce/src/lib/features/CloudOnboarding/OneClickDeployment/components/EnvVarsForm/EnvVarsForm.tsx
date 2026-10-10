import React, { useEffect } from 'react';
import { GraphQLError } from 'graphql';
import { useGetTenantEnvs, useUpdateTenantEnv } from './hooks';
import { getFormProperties } from './utils';
import { EnvVarsFormState, RequiredEnvVar } from '../../types';
import { UpdateEnvObj } from './types';
import { EnvVarsFormFields, CustomFooter } from './components';
import { SimpleForm, Dialog, Text } from '@hasura/shared/ui';
import { useErrorNotification } from '@hasura/metadata/api';
import { Flex, Heading } from '@radix-ui/themes';

export type EnvVarsFormProps = {
  envVars: RequiredEnvVar[];
  formState: EnvVarsFormState;
  setFormState: React.Dispatch<React.SetStateAction<EnvVarsFormState>>;
  successCb?: () => void;
  errorCb?: () => void;
};

export function EnvVarsForm(props: EnvVarsFormProps) {
  const { envVars, formState, setFormState, successCb } = props;
  const showErrorNotification = useErrorNotification();

  const { data: tenantEnvData } = useGetTenantEnvs();

  useEffect(() => {
    if (
      tenantEnvData &&
      tenantEnvData.errors &&
      tenantEnvData.errors.length > 0
    ) {
      showErrorNotification({
        title: 'Error fetching Environment Variables',
        error: tenantEnvData.errors[0],
      });
    }
  }, [tenantEnvData]);

  const tenantHash = tenantEnvData?.data?.getTenantEnv?.hash;
  const tenantEnvVars = tenantEnvData?.data?.getTenantEnv?.envVars;

  const { schema, defaultValues } = React.useMemo(
    () => getFormProperties(envVars, tenantEnvVars),
    [envVars, tenantEnvVars],
  );

  const updateTenantEnvSuccessCb = () => {
    if (successCb) successCb();
  };

  const updateTenantEnvErrorCb = (error?: GraphQLError) => {
    setFormState('error');
    showErrorNotification({
      title: 'Error updating Environment Variables',
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

      const envVar = envVars.find((env) => k === env.Name);
      if (envVar?.ValueType === 'STRING_ARRAY') {
        formData[k] = JSON.stringify(
          (v as string).split(',').map((d) => d.trim()),
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
    <Dialog size="xl">
      <SimpleForm
        schema={schema}
        onSubmit={onSubmit}
        options={{ defaultValues }}
      >
        <Flex
          direction="column"
          gap="4"
          className="max-h-[calc(100vh-20rem)] overflow-y-auto"
        >
          <Heading size="6">Environment Variables</Heading>
          <Text>
            The following variables are required to set up your project.
          </Text>

          <EnvVarsFormFields envVars={envVars} />
        </Flex>
        <CustomFooter formState={formState} />
      </SimpleForm>
    </Dialog>
  );
}
