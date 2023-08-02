import React from 'react';
import { CardRadioGroup } from '../../../../../new-components/CardRadioGroup';

export type RemoteRelOption = 'remoteSchema' | 'remoteDB';

interface RelationshipTypeCardRadioGroupProps {
  value: RemoteRelOption;
  onChange: (option: RemoteRelOption) => void;
}

const items: { value: RemoteRelOption; title: string; body: string }[] = [
  {
    value: 'remoteSchema',
    title: 'Remote Schema',
    body: 'Relationship from this remote schema to another remote schema.',
  },
  {
    value: 'remoteDB',
    title: 'Remote Database',
    body: 'Relationship from this remote schema to a remote database table.',
  },
];

export const RelationshipTypeCardRadioGroup = (
  props: RelationshipTypeCardRadioGroupProps
) => {
  const { value = 'remoteSchema', onChange } = props;
  return (
    <CardRadioGroup<RemoteRelOption>
      items={items}
      value={value}
      onChange={onChange}
    />
  );
};
