import { BooleanInput } from './BooleanInput';

export const UsePreparedStatements = ({ name }: { name: string }) => {
  return (
    <BooleanInput
      name={name}
      label="Use Prepared Statements"
      tooltip="Prepared statements are disabled by default"
    />
  );
};
