import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { RelationshipRow, RelationshipRowProps } from './RelationshipRow';
import { action } from '@storybook/addon-actions';

export default {
  component: RelationshipRow,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof RelationshipRow>;

const relationship: SuggestedRelationshipWithName = {
  constraintName: 'Album_Artist',
  type: 'object',
  from: {
    table: 'Album',
    columns: ['id'],
  },
  to: {
    table: 'Artist',
    columns: ['albumId'],
  },
};

const baseProps: RelationshipRowProps = {
  relationship: relationship,
  dataSourceName: 'Chinook',
  isChecked: false,
  isLoading: false,
  onCustomize: () => action('onCustomize')(),
  onToggle: () => action('onToggle')(),
  onTrack: async () => action('onTrack')(),
};

export const Base: StoryFn<typeof RelationshipRow> = () => (
  <RelationshipRow {...baseProps} />
);

export const Checked: StoryFn<typeof RelationshipRow> = () => (
  <RelationshipRow {...baseProps} isChecked />
);
