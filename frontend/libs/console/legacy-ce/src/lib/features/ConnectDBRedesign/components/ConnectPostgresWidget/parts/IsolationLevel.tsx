import { SelectField } from '@hasura/shared/ui';

export const IsolationLevel = ({ name }: { name: string }) => {
  return (
    <SelectField
      options={[
        {
          value: 'read-committed',
          label: 'read-committed',
        },
        {
          value: 'repeatable-read',
          label: 'repeatable-read',
        },
        {
          value: 'serializable',
          label: 'serializable',
        },
      ]}
      name={name}
      placeholder="-- Select --"
      label="Isolation Level"
      tooltip="The transaction isolation level in which the queries made to the source will be run"
    />
  );
};
