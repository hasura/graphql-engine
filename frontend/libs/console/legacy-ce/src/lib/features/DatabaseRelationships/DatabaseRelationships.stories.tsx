import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { waitFor, within, userEvent } from '@storybook/testing-library';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { DatabaseRelationships } from './DatabaseRelationships';
import {
  handlers,
  trackedArrayRelationshipsHandlers,
} from './mocks/handler.mock';

export default {
  component: DatabaseRelationships,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof DatabaseRelationships>;

export const Basic: StoryObj<typeof DatabaseRelationships> = {
  render: () => (
    <DatabaseRelationships
      dataSourceName="aPostgres"
      table={{ name: 'Album', schema: 'public' }}
    />
  ),
  parameters: {
    msw: trackedArrayRelationshipsHandlers(),
  },
};

export const Testing: StoryObj<typeof DatabaseRelationships> = {
  name: '🧪 Test - Tracked array relationships',
  render: () => (
    <DatabaseRelationships
      dataSourceName="aPostgres"
      table={{ name: 'Album', schema: 'public' }}
    />
  ),
  parameters: {
    msw: trackedArrayRelationshipsHandlers(),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(
      async () => {
        expect(await canvas.findByText('NAME')).toBeVisible();
      },
      { timeout: 10000 }
    );

    const firstRelationship = await canvas.findByText('albumAlbumCovers');
    expect(firstRelationship).toBeVisible();

    const secondRelationship = await canvas.findByText('albumTracks');
    expect(secondRelationship).toBeVisible();

    const arrayRelationships = await canvas.findAllByText('Array');
    expect(arrayRelationships).toHaveLength(2);

    expect(await canvas.findByText('AlbumCovers')).toBeVisible();
    expect(await canvas.findByText('Track')).toBeVisible();

    expect(await canvas.findAllByText('Rename')).toHaveLength(2);

    await waitFor(async () => {
      expect(await canvas.findAllByText('SOURCE')).toHaveLength(2);
      expect(await canvas.findAllByText('TYPE')).toHaveLength(2);
      expect(await canvas.findAllByText('RELATIONSHIP')).toHaveLength(2);

      expect(await canvas.findByText('SUGGESTED RELATIONSHIPS')).toBeVisible();
      expect(await canvas.findByText('artist')).toBeVisible();
      expect(await canvas.findAllByText('Object')).toHaveLength(2);
      expect(await canvas.findAllByText('Artist')).toHaveLength(2);

      expect(await canvas.findByText('Add')).toBeVisible();
    });

    expect(await canvas.findByText('Remote Schema')).toBeVisible();
    expect(await canvas.findByText('Type')).toBeVisible();
    expect(await canvas.findByText('Field')).toBeVisible();
    expect(await canvas.findByText('Database')).toBeVisible();
    expect(await canvas.findByText('Table')).toBeVisible();
    expect(await canvas.findByText('Column')).toBeVisible();

    expect(await canvas.findByText('Add Relationship')).toBeVisible();

    // click "Remove" button
    userEvent.click((await canvas.findAllByText('Remove'))[0]);

    expect(await canvas.findByText('Confirm Action')).toBeVisible();
    expect(await canvas.findByText('Drop Relationship')).toBeVisible();

    userEvent.click(await canvas.findByText('Cancel'));

    // click "Add" button
    userEvent.click(await canvas.findByText('Add'));

    expect(await canvas.findByText('Track relationship: artist')).toBeVisible();
    expect(await canvas.findByText('Track relationship')).toBeVisible();

    userEvent.click(await canvas.findByText('Cancel'));

    // click "Add Relationship" button
    userEvent.click(await canvas.findByText('Add Relationship'));

    expect(
      await canvas.findByText('Create Relationship', { selector: 'h2' })
    ).toBeVisible();

    expect(await canvas.findByText('Relationship Name')).toBeVisible();

    await waitFor(async () => {
      expect(await canvas.findByText('From Source')).toBeVisible();

      expect(await canvas.findByText('To Reference')).toBeVisible();

      expect(
        await canvas.findByText('Create Relationship', { selector: 'span' })
      ).toBeVisible();

      userEvent.click(await canvas.findByText('Close'));
    });
  },
};
