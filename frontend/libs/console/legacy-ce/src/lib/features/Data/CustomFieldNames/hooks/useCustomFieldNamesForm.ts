import { useConsoleForm } from '../../../../new-components/Form';
import { implement } from '../../../../utils/zodUtils';
import React from 'react';
import { useWatch } from 'react-hook-form';
import { z } from 'zod';
import { CustomFieldNamesFormProps } from '../CustomFieldNamesForm';
import { CustomFieldNamesFormVals } from '../types';
import {
  buildConfigFromFormValues,
  customFieldNamesPlaceholders,
  initFormValues,
  mutation_field_props,
  query_field_props,
} from '../utils';

const schema = implement<CustomFieldNamesFormVals>().with({
  custom_name: z.string(),
  select: z.string(),
  select_by_pk: z.string(),
  select_aggregate: z.string(),
  select_stream: z.string(),
  insert: z.string(),
  insert_one: z.string(),
  update: z.string(),
  update_by_pk: z.string(),
  delete: z.string(),
  delete_by_pk: z.string(),
  update_many: z.string(),
  logical_model: z.string(),
});

export const useCustomFieldNamesForm = ({
  initialTableName,
  currentConfiguration,
  onSubmit,
}: Omit<CustomFieldNamesFormProps, 'onClose'>) => {
  const { methods, Form } = useConsoleForm({
    schema,
    options: { defaultValues: initFormValues(currentConfiguration) },
  });

  const {
    formState: { errors },
    watch,
  } = methods;

  const handleSubmit = (data: CustomFieldNamesFormVals) => {
    onSubmit(data, buildConfigFromFormValues(data));
  };

  const customTableName = useWatch({
    control: methods.control,
    name: 'custom_name',
  });

  const placeholders = customFieldNamesPlaceholders(
    customTableName || initialTableName
  );

  const values = watch();

  const isQueryOpen = React.useMemo(
    () =>
      Object.entries(currentConfiguration?.custom_root_fields || {}).some(
        entry => {
          const key = entry[0] as keyof CustomFieldNamesFormVals;
          const [, value] = entry;
          return query_field_props.includes(key) && !!value;
        }
      ),

    [currentConfiguration?.custom_root_fields]
  );

  const isMutateOpen = React.useMemo(
    () =>
      Object.entries(currentConfiguration?.custom_root_fields || {}).some(
        entry => {
          const key = entry[0] as keyof CustomFieldNamesFormVals;
          const [, value] = entry;
          return mutation_field_props.includes(key) && !!value;
        }
      ),
    [currentConfiguration?.custom_root_fields]
  );

  const hasValues = React.useMemo(
    () => Object.values(values).some(value => !!value),
    [values]
  );

  const reset = () => {
    methods.reset(initFormValues());
  };

  return {
    methods,
    Form,
    errors,
    placeholders,
    handleSubmit,
    isQueryOpen,
    isMutateOpen,
    hasValues,
    reset,
  };
};
