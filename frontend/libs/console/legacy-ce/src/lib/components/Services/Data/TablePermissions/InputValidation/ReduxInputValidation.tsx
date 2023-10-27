import React, { useEffect } from 'react';
import z from 'zod';
import { useConsoleForm } from '../../../../../new-components/Form';
import { InputValidation, inputValidationSchema } from './InputValidation';
import { ValidationInput } from '../../../../../metadata/types';

type UpdateFunction = (values: z.infer<typeof inputValidationSchema>) => void;

type ReduxInputValidationProps = {
  updateFunction: UpdateFunction;
  permissionsState: ValidationInput;
};

export const ReduxInputValidation = (props: ReduxInputValidationProps) => {
  const { updateFunction, permissionsState } = props;

  const {
    methods: { watch },
    Form,
  } = useConsoleForm({
    schema: inputValidationSchema,
    options: {
      defaultValues: {
        enabled: !!permissionsState,
        type: 'http',
        definition: {
          url: permissionsState?.definition.url,
          forward_client_headers:
            permissionsState?.definition.forward_client_headers,
          headers: permissionsState?.definition.headers,
          timeout: permissionsState?.definition.timeout ?? undefined,
        },
      },
    },
  });
  const values = watch();
  useEffect(() => {
    if (Object.keys(values).length > 0) updateFunction(values as any);
  }, [JSON.stringify(values)]);

  return (
    <Form onSubmit={() => {}}>
      <InputValidation />
    </Form>
  );
};
