import { useConsoleForm } from '@hasura/shared/ui';
import React from 'react';
import { z } from 'zod';
import { CustomSchemaFormProps } from '../CustomSchemaForm';
import { CustomSchemaFormVals } from '../types';

const schema = z.object({
  // A `type: 'number'` input stores a number (NaN when empty); keep the
  // string form the rest of this form uses.
  schemaSamplingSize: z.preprocess(
    (value) =>
      typeof value === 'number'
        ? Number.isNaN(value)
          ? ''
          : String(value)
        : value,
    z.string(),
  ),
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
    () => Object.values(values).some((value) => !!value),
    [values],
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
