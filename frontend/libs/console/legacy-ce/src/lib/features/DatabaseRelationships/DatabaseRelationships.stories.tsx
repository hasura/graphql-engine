import { expect, userEvent, waitFor, within } from 'storybook/test';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
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
      source={{ name: 'aPostgres', kind: 'postgres' }}
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
      source={{ name: 'aPostgres', kind: 'postgres' }}
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
        await expect(await canvas.findByText('NAME')).toBeVisible();
      },
      { timeout: 10000 },
    );

    const firstRelationship = await canvas.findByText('albumAlbumCovers');
    await expect(firstRelationship).toBeVisible();

    const secondRelationship = await canvas.findByText('albumTracks');
    await expect(secondRelationship).toBeVisible();

    const arrayRelationships = await canvas.findAllByText('Array');
    await expect(arrayRelationships).toHaveLength(2);

    await expect(await canvas.findByText('public.AlbumCovers')).toBeVisible();
    await expect(await canvas.findByText('public.Track')).toBeVisible();

    await expect(await canvas.findAllByText('Rename')).toHaveLength(2);

    await waitFor(async () => {
      await expect(await canvas.findAllByText('SOURCE')).toHaveLength(2);
      await expect(await canvas.findAllByText('TYPE')).toHaveLength(2);
      await expect(await canvas.findAllByText('RELATIONSHIP')).toHaveLength(2);

      await expect(
        await canvas.findByText('SUGGESTED RELATIONSHIPS'),
      ).toBeVisible();
      await expect(await canvas.findByText('artist')).toBeVisible();
      await expect(await canvas.findAllByText('Object')).toHaveLength(2);
      await expect(await canvas.findAllByText('public.Artist')).toHaveLength(1);
      await expect(await canvas.findAllByText('dbo.Artist')).toHaveLength(1);
      await expect(await canvas.findByText('Add')).toBeVisible();
    });

    await expect(await canvas.findByText('Remote Schema')).toBeVisible();
    await expect(await canvas.findByText('Type')).toBeVisible();
    await expect(await canvas.findByText('Field')).toBeVisible();
    await expect(await canvas.findByText('Database')).toBeVisible();
    await expect(await canvas.findByText('Table')).toBeVisible();
    await expect(await canvas.findByText('Column')).toBeVisible();

    await expect(await canvas.findByText('Add Relationship')).toBeVisible();

    // click "Remove" button
    await userEvent.click((await canvas.findAllByText('Remove'))[0]);

    await expect(await canvas.findByText('Confirm Action')).toBeVisible();
    await expect(await canvas.findByText('Drop Relationship')).toBeVisible();

    await userEvent.click(await canvas.findByText('Cancel'));

    // click "Add" button
    await userEvent.click(await canvas.findByText('Add'));

    await expect(
      await canvas.findByText('Track relationship: artist'),
    ).toBeVisible();
    await expect(await canvas.findByText('Track relationship')).toBeVisible();

    await userEvent.click(await canvas.findByText('Cancel'));

    // click "Add Relationship" button
    await userEvent.click(await canvas.findByText('Add Relationship'));

    await expect(
      await canvas.findByText('Create Relationship', { selector: 'h2' }),
    ).toBeVisible();

    await expect(await canvas.findByText('Relationship Name')).toBeVisible();

    await waitFor(async () => {
      await expect(await canvas.findByText('From Source')).toBeVisible();

      await expect(await canvas.findByText('To Reference')).toBeVisible();

      await expect(
        await canvas.findByText('Create Relationship', { selector: 'span' }),
      ).toBeVisible();

      await userEvent.click(await canvas.findByText('Close'));
    });
  },
};
