import { OpenApiSchema } from '@hasura/dc-api-types';
import { getInputAttributes } from '../utils';
import { InputField } from '../../Form';

export const isTextInputField = (configSchema: OpenApiSchema): boolean => {
  const type = configSchema.type;

  /**
   * if its of type 'string'
   */
  if (type === 'string') return true;

  return false;
};

export const TextInputField = ({
  name,
  configSchema,
}: {
  name: string;
  configSchema: OpenApiSchema;
}) => {
  const { tooltip, label } = getInputAttributes(name, configSchema);

  return (
    <InputField
      fieldProps={{ type: 'text' }}
      name={name}
      label={label}
      tooltip={tooltip}
    />
  );
};
