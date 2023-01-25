import { TableTrackingCustomizationFormProps } from '@/components/Services/Data/Schema/tableTrackCustomization/TableTrackingCustomizationForm';
import { TrackingTableFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/types';
import { buildConfigFromFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/utils';
import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import {
  buildDefaults,
  emptyForm,
  getTrackingTableFormPlaceholders,
  mutation_field_props,
  query_field_props,
} from '../utils';

export const useGqlCustomizationForm = ({
  initialTableName,
  currentConfiguration,
  onSubmit,
}: Omit<TableTrackingCustomizationFormProps, 'onClose'>) => {
  const formMethods = useForm<TrackingTableFormValues>({
    defaultValues: buildDefaults(currentConfiguration),
  });

  const {
    formState: { errors },
    watch,
  } = formMethods;

  const handleSubmit = (data: TrackingTableFormValues) => {
    onSubmit(data, buildConfigFromFormValues(data));
  };

  const [customTableName, setCustomTableName] = useState('');

  const placeholders = getTrackingTableFormPlaceholders(
    customTableName || initialTableName
  );

  const values = watch();

  const isQueryOpen = React.useMemo(
    () =>
      Object.entries(currentConfiguration?.custom_root_fields || {}).some(
        entry => {
          const key = entry[0] as keyof TrackingTableFormValues;
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
          const key = entry[0] as keyof TrackingTableFormValues;
          const [, value] = entry;
          return mutation_field_props.includes(key) && !!value;
        }
      ),
    [currentConfiguration?.custom_root_fields]
  );

  const hasValues = React.useMemo(
    () => Object.entries(values).some(([, value]) => !!value),
    [values]
  );

  const reset = () => {
    formMethods.reset(emptyForm);
    setCustomTableName('');
  };

  return {
    formMethods,
    errors,
    placeholders,
    handleSubmit,
    isQueryOpen,
    isMutateOpen,
    hasValues,
    reset,
    setCustomTableName,
  };
};
