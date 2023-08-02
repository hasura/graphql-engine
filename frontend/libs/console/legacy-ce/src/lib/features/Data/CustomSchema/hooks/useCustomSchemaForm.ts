import { useConsoleForm } from '../../../../new-components/Form';
import { implement } from '../../../../utils/zodUtils';
import React from 'react';
import { z } from 'zod';
import { CustomSchemaFormProps } from '../CustomSchemaForm';
import { CustomSchemaFormVals } from '../types';

const schema = implement<CustomSchemaFormVals>().with({
  schemaSamplingSize: z.string(),
  schemaType: z.enum(['json', 'graphql']),
  graphqlSchema: z.string().optional(),
  jsonSchema: z.string().optional(),
});

export const useCustomSchemaForm = ({
  onSubmit,
  jsonSchema,
  graphqlSchema,
}: Omit<CustomSchemaFormProps, 'onClose'>) => {
  const { methods, Form } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        schemaType: 'json',
        schemaSamplingSize: '1000',
        jsonSchema,
        graphqlSchema,
      },
    },
  });

  const {
    formState: { errors },
    watch,
  } = methods;

  const handleSubmit = (data: CustomSchemaFormVals) => {
    onSubmit(data);
  };

  const values = watch();

  const hasValues = React.useMemo(
    () => Object.values(values).some(value => !!value),
    [values]
  );

  const reset = () => {
    methods.reset({
      schemaType: 'json',
      schemaSamplingSize: '1000',
      jsonSchema,
      graphqlSchema,
    });
  };

  return {
    methods,
    Form,
    errors,
    handleSubmit,
    hasValues,
    reset,
  };
};
