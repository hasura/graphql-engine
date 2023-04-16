import { ComponentStory, ComponentMeta } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { expect } from '@storybook/jest';

import { ManageTrackedRelationships } from '../components/ManageTrackedRelationships';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { Relationship } from '../../../DatabaseRelationships';

export default {
  title: 'Data/Components/ManageTrackedRelationships',
  component: ManageTrackedRelationships,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof ManageTrackedRelationships>;

const suggestedRelationships: SuggestedRelationshipWithName[] = [];

const trackedFKRelationships: Relationship[] = [];

export const Base: ComponentStory<typeof ManageTrackedRelationships> = () => (
  <ManageTrackedRelationships
    dataSourceName="chinook"
    suggestedRelationships={suggestedRelationships}
    trackedFKRelationships={trackedFKRelationships}
    isLoading={false}
  />
);

Base.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // Reset initial metadata to make sure tests start from a clean slate everytime

  userEvent.click(await canvas.findByText('Foreign Key Relationships'));

  await expect(
    canvas.getByText('No untracked relationships found')
  ).toBeInTheDocument();
};
