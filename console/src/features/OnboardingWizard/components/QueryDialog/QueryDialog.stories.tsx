import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { QueryDialog } from './QueryDialog';
import type { Props } from './QueryDialog';

export default {
  title: 'features/Onboarding Wizard/Query Dialog',
  component: QueryDialog,
  argTypes: {
    onRunHandler: { action: true },
    onSkipHandler: { action: true },
  },
} as ComponentMeta<typeof QueryDialog>;

const defaultQuery = `
# Lookup artist info, albums, tracks based on relations
# Filter for only 'ArtistId' with the ID of '22'

query lookupArtist {
    sample_Artist(where: {ArtistId: {_eq: 22}}) {
            ArtistId
            Name
            Albums {
            AlbumId
            Title
            Tracks {
                TrackId
                Name
            }
        }
    }
}
`;

export const Base: Story<Props> = args => (
  <QueryDialog
    title={args.title}
    description={args.description}
    query={args.query}
    schemaImage={args.schemaImage}
    onRunHandler={args.onRunHandler}
    onSkipHandler={args.onSkipHandler}
  />
);

Base.args = {
  title: '👋 Welcome to Hasura!',
  description: 'Get started learning Hasura with an example.',
  query: defaultQuery,
  schemaImage:
    'https://raw.githubusercontent.com/hasura/template-gallery/main/postgres/getting-started/diagram.png',
};

Base.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);
  const runButton = canvas.getByText('Run Query');
  const skipButton = canvas.getByText('Skip getting started tutorial');

  // Expect element renders successfully
  expect(canvas.getByText('👋 Welcome to Hasura!')).toBeVisible();
  expect(
    canvas.getByText('Get started learning Hasura with an example.')
  ).toBeVisible();
  // Expect button to be present in the dialog
  expect(runButton).toBeInTheDocument();
  expect(runButton).not.toBeDisabled();
  expect(skipButton).toBeInTheDocument();
  expect(canvas.getByTestId('query-dialog-schema-image')).toBeVisible();
  expect(canvas.getByTestId('query-dialog-sample-query')).toBeVisible();
  await userEvent.click(runButton);
  expect(args.onRunHandler).toBeCalledTimes(1);
  await userEvent.click(skipButton);
  expect(args.onSkipHandler).toBeCalledTimes(1);
};
