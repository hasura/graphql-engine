import { StoryFn, StoryObj, Meta } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { expect } from '@storybook/jest';
import { TrackedRelationships } from './TrackedRelationships';
import { action } from '@storybook/addon-actions';
import { Relationship } from '../../../DatabaseRelationships';

export default {
  title: 'Data/Components/TrackedRelationships',
  component: TrackedRelationships,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof TrackedRelationships>;

const relationships: Relationship[] = [
  {
    name: 'CUSTOMER_INVOICEs',
    fromSource: 'Snow',
    fromTable: ['CUSTOMER'],
    relationshipType: 'Array',
    type: 'localRelationship',
    definition: { mapping: {}, toTable: 'INVOICE' },
  },
  {
    name: 'INVOICE_CUSTOMER',
    fromSource: 'Snow',
    fromTable: ['INVOICE'],
    relationshipType: 'Object',
    type: 'localRelationship',
    definition: {
      toTable: ['CUSTOMER'],
      mapping: { CUSTOMERID: 'CUSTOMERID' },
    },
  },
];

export const Base: StoryObj<typeof TrackedRelationships> = {
  render: () => (
    <TrackedRelationships
      dataSourceName="chinook"
      isLoading={false}
      onRefetchMetadata={() => action('onRefetchMetadata')()}
      relationships={relationships}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText('Untrack Selected (0)')).toBeInTheDocument();
    await expect(canvas.getByText('Show 10 relationships')).toBeInTheDocument();
    await expect(canvas.getByText('RELATIONSHIP NAME')).toBeInTheDocument();
    await expect(canvas.getByText('SOURCE')).toBeInTheDocument();
    await expect(canvas.getByText('TYPE')).toBeInTheDocument();
    await expect(canvas.getByText('RELATIONSHIP')).toBeInTheDocument();

    await expect(canvas.getByText('CUSTOMER_INVOICEs')).toBeInTheDocument();
    await expect(canvas.getByText('INVOICE_CUSTOMER')).toBeInTheDocument();

    await expect(canvas.getByText('Array')).toBeInTheDocument();
    await expect(canvas.getByText('Object')).toBeInTheDocument();

    await expect(canvas.getAllByText('Snow')).toHaveLength(2);
    await expect(canvas.getAllByText('Rename')).toHaveLength(2);
    await expect(canvas.getAllByText('Remove')).toHaveLength(2);
  },
};

export const Loading: StoryFn<typeof TrackedRelationships> = () => (
  <TrackedRelationships
    dataSourceName="chinook"
    isLoading={true}
    onRefetchMetadata={() => action('onRefetchMetadata')()}
    relationships={relationships}
  />
);

export const NoRelationships: StoryFn<typeof TrackedRelationships> = () => (
  <TrackedRelationships
    dataSourceName="chinook"
    isLoading={false}
    onRefetchMetadata={() => action('onRefetchMetadata')()}
    relationships={[]}
  />
);
