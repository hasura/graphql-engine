import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Relationship } from '@/features/DatabaseRelationships';
import { expect } from '@storybook/jest';
import { waitFor, within } from '@storybook/testing-library';
import { action } from '@storybook/addon-actions';
import { ReactTableWrapper } from './ReactTableWrapper';
import { handlers } from '../../../__mocks__/handlers.mock';

export default {
  component: ReactTableWrapper,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as ComponentMeta<typeof ReactTableWrapper>;

const mockDataForRows = [
  {
    AlbumId: 1,
    Title: 'For Those About To Rock We Salute You',
    ArtistId: 1,
  },
  { AlbumId: 2, Title: 'Balls to the Wall', ArtistId: 2 },
  { AlbumId: 3, Title: 'Restless and Wild', ArtistId: 2 },
  { AlbumId: 4, Title: 'Let There Be Rock', ArtistId: 1 },
  { AlbumId: 5, Title: 'Big Ones', ArtistId: 3 },
  { AlbumId: 6, Title: 'Jagged Little Pill', ArtistId: 4 },
  { AlbumId: 7, Title: 'Facelift', ArtistId: 5 },
  { AlbumId: 8, Title: 'Warner 25 Anos', ArtistId: 6 },
  { AlbumId: 9, Title: 'Plays Metallica By Four Cellos', ArtistId: 7 },
  { AlbumId: 10, Title: 'Audioslave', ArtistId: 8 },
];

export const Default: ComponentStory<typeof ReactTableWrapper> = () => {
  return (
    <ReactTableWrapper
      rows={mockDataForRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
    />
  );
};

export const Basic: ComponentStory<typeof ReactTableWrapper> = () => {
  return (
    <ReactTableWrapper
      rows={mockDataForRows}
      isRowsSelectionEnabled
      onRowsSelect={action('onRowsSelect')}
    />
  );
};

export const SelectionDisabled: ComponentStory<typeof ReactTableWrapper> =
  () => {
    return (
      <ReactTableWrapper
        rows={mockDataForRows}
        isRowsSelectionEnabled={false}
        onRowsSelect={action('onRowsSelect')}
      />
    );
  };

Basic.storyName = '🧪 Test - Basic data with columns';

Basic.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  waitFor(
    async () => {
      const albumRows = await canvas.findAllByTestId(/^@table-row-.*$/);
      expect(albumRows.length).toBe(10);

      const firstRowOfAlbum = await canvas.findAllByTestId(
        /^@table-cell-0-.*$/
      );
      expect(firstRowOfAlbum.length).toBe(4);

      expect(firstRowOfAlbum[1]).toHaveTextContent('1'); // AlbumId
      expect(firstRowOfAlbum[2]).toHaveTextContent(
        'For Those About To Rock We Salute You'
      );
    },
    { timeout: 5000 }
  );
};

export const WithRelationships: ComponentStory<typeof ReactTableWrapper> =
  () => {
    const relationships: Relationship[] = [
      {
        name: 'Artist',
        fromSource: 'sqlite_test',
        fromTable: ['Album'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Artist'],
          mapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
      {
        name: 'Tracks',
        fromSource: 'sqlite_test',
        fromTable: ['Album'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          mapping: {
            AlbumId: 'AlbumId',
          },
        },
      },
    ];

    return (
      <ReactTableWrapper
        rows={mockDataForRows}
        relationships={{
          allRelationships: relationships,
          onClick: () => {},
          onClose: () => {},
        }}
        isRowsSelectionEnabled
        onRowsSelect={action('onRowsSelect')}
      />
    );
  };

WithRelationships.storyName = '🧪 Test - Data with Relationships';

WithRelationships.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  waitFor(
    async () => {
      const albumRows = await canvas.findAllByTestId(/^@table-row-.*$/);
      expect(albumRows.length).toBe(10);

      const firstRowOfAlbum = await canvas.findAllByTestId(
        /^@table-cell-0-.*$/
      );
      expect(firstRowOfAlbum.length).toBe(5);

      expect(firstRowOfAlbum[0]).toHaveTextContent('1'); // AlbumId
      expect(firstRowOfAlbum[1]).toHaveTextContent(
        'For Those About To Rock We Salute You'
      );

      // The last two columns should be relationships
      expect(firstRowOfAlbum[3]).toHaveTextContent('View');
      expect(firstRowOfAlbum[4]).toHaveTextContent('View');
    },
    { timeout: 5000 }
  );
};
