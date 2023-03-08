import { Select } from '../../../../../new-components/Form';

export const IsolationLevel = ({ name }: { name: string }) => {
  return (
    <Select
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
      label="Isolation Level"
      tooltip="The transaction isolation level in which the queries made to the source will be run"
    />
  );
};
