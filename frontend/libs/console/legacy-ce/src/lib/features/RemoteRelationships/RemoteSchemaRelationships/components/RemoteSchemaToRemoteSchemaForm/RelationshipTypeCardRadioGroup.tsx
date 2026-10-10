import { RadioCardGroup, Text } from '@hasura/shared/ui';
import { ReactNode } from 'react';

export type RemoteRelOption = 'remoteSchema' | 'remoteDB';

interface RelationshipTypeCardRadioGroupProps {
  value: RemoteRelOption;
  onChange: (option: RemoteRelOption) => void;
}

const items: { value: RemoteRelOption; label: ReactNode }[] = [
  {
    value: 'remoteSchema',
    label: (
      <div>
        <Text as="div" weight="bold">
          Remote Schema
        </Text>
        <Text as="div">
          Relationship from this remote schema to another remote schema.
        </Text>
      </div>
    ),
  },
  {
    value: 'remoteDB',
    label: (
      <div>
        <Text as="div" weight="bold">
          Remote Database
        </Text>
        <Text as="div">
          Relationship from this remote schema to a remote database table.
        </Text>
      </div>
    ),
  },
];

export const RelationshipTypeCardRadioGroup = ({
  value = 'remoteSchema',
  onChange,
}: RelationshipTypeCardRadioGroupProps) => {
  return (
    <RadioCardGroup
      options={items}
      value={value}
      onChange={onChange as (option: string) => void}
    />
  );
};
