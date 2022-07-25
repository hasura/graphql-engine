import React from 'react';
import ReactJson from 'react-json-view';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { Story, Meta } from '@storybook/react';
import { handlers } from '../mocks/handlers.mock';
import { useSuggestedRelationships } from '../useSuggestedRelationships';

const target = {
  database: 'default',
  schema: 'public',
};

const UseSuggestedRelationshipsComponent = () => {
  const { data: arrayRelationships } = useSuggestedRelationships({
    ...target,
    table: 'user',
  });

  const { data: objectRelationships } = useSuggestedRelationships({
    ...target,
    table: 'product',
  });

  if (!arrayRelationships || !objectRelationships) {
    return <div>No relationships</div>;
  }

  return (
    <>
      <p>Suggested object relationships</p>
      <ReactJson src={objectRelationships} />
      <p>Suggested array relationships</p>
      <ReactJson src={arrayRelationships} />
    </>
  );
};

const UseSuggestedObjectRelationshipsComponent = () => {
  const tableName = 'product';
  const { data: objectRelationships } = useSuggestedRelationships({
    ...target,
    table: tableName,
  });

  if (!objectRelationships) {
    return <div>No relationships</div>;
  }

  return (
    <>
      <h3 className="text-lg">
        <strong>Table Name: {tableName}</strong>
      </h3>
      <p>Suggested object relationships</p>
      <ReactJson src={objectRelationships} />
    </>
  );
};

const UseSuggestedArrayRelationshipsComponent = () => {
  const tableName = 'user';
  const { data: arrayRelationships } = useSuggestedRelationships({
    ...target,
    table: tableName,
  });

  if (!arrayRelationships) {
    return <div>No relationships</div>;
  }

  return (
    <>
      <h3 className="text-lg">
        <strong>Table Name: {tableName}</strong>
      </h3>
      <p>Suggested array relationships</p>
      <ReactJson src={arrayRelationships} />
    </>
  );
};

export default {
  title: 'Features/Data Relationships/useSuggestedRelationships',
  decorators: [ReactQueryDecorator()],
  component: UseSuggestedRelationshipsComponent,
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const AllSuggested: Story = () => <UseSuggestedRelationshipsComponent />;
export const ObjectSuggested: Story = () => (
  <UseSuggestedObjectRelationshipsComponent />
);
export const ArraySuggested: Story = () => (
  <UseSuggestedArrayRelationshipsComponent />
);

export const WithExistingRelationships: Story = () => (
  <>
    <p>This should have no suggested relationships as they already exist</p>
    <UseSuggestedRelationshipsComponent />
  </>
);
WithExistingRelationships.parameters = {
  msw: handlers({ withExisting: true }),
};
