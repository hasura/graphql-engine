import { expect } from '@storybook/jest';
import { StoryObj, Meta } from '@storybook/react';
import { within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';

import { Relationship } from '../../../DatabaseRelationships';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { ManageTrackedRelationships } from '../components/ManageTrackedRelationships';

export default {
  title: 'Data/Components/ManageTrackedRelationships',
  component: ManageTrackedRelationships,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof ManageTrackedRelationships>;

const suggestedRelationships: SuggestedRelationshipWithName[] = [];

const trackedFKRelationships: Relationship[] = [];

export const Base: StoryObj<typeof ManageTrackedRelationships> = {
  render: () => (
    <ManageTrackedRelationships
      dataSourceName="chinook"
      suggestedRelationships={suggestedRelationships}
      trackedFKRelationships={trackedFKRelationships}
      isLoading={false}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(
      canvas.getByText('No untracked relationships found')
    ).toBeInTheDocument();
  },
};
