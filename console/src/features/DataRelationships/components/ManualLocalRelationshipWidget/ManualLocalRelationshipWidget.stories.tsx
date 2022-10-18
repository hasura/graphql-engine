import React, { useState } from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ManualLocalRelationshipWidget } from './ManualLocalRelationshipWidget';
import { handlers } from './__mocks__/localrelationships.mock';

export default {
  title: 'Relationships/Manual Relationship  🧬',
  component: ManualLocalRelationshipWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: Story<ManualLocalRelationshipWidget> = () => {
  const [formState, updateFormState] = useState('');
  return (
    <div className="w-2/3">
      <ManualLocalRelationshipWidget
        dataSourceName="sqlite_test"
        table={['Album']}
        onSuccess={() => {
          updateFormState('success');
        }}
      />
      <div>{formState}</div>
    </div>
  );
};
