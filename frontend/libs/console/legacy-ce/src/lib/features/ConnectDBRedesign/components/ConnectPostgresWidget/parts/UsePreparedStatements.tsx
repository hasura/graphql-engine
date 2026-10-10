import { SwitchField } from '@hasura/shared/ui';

export const UsePreparedStatements = ({ name }: { name: string }) => {
  return (
    <SwitchField
      name={name}
      label="Use Prepared Statements"
      tooltip="Prepared statements are disabled by default"
    />
  );
};
