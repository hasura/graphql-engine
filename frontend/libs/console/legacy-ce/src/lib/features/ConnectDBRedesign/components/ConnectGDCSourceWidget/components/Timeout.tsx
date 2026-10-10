import { InputField } from '@hasura/shared/ui';

export const Timeout = ({ name }: { name: string }) => {
  return (
    <InputField
      name={name}
      label="Timeout (in seconds)"
      fieldProps={{
        type: 'number',
        placeholder: 'In Seconds',
      }}
    />
  );
};
