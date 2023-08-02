import { Select } from '../../../new-components/Form';
import { OpenApiSchema } from '@hasura/dc-api-types';
import React from 'react';
import { getInputAttributes } from '../utils';

export const isEnumInputField = (configSchema: OpenApiSchema): boolean => {
  const type = configSchema.type;
  const enumValues = configSchema.enum;

  /**
   * if its of type 'string'
   */
  return type === 'string' && !!enumValues;
};

export const EnumInputField = ({
  name,
  configSchema,
}: {
  name: string;
  configSchema: OpenApiSchema;
}) => {
  const { tooltip, label } = getInputAttributes(name, configSchema);

  return (
    <Select
      name={name}
      label={label}
      tooltip={tooltip}
      options={(configSchema.enum ?? []).map(_enum => ({
        label: _enum ?? 'null',
        value: _enum ?? 'null',
      }))}
    />
  );
};
