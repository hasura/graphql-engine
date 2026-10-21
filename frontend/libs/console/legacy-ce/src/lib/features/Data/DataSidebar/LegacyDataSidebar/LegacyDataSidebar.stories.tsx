import { useLayoutEffect, useState } from 'react';
import type { Decorator, Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, userEvent, within } from 'storybook/test';
import { http, HttpResponse, delay } from 'msw';
import { Route, Routes, useNavigate } from 'react-router';
import { AppContext } from '@hasura/shared/context';
import { Metadata, MetadataTable } from '@hasura/shared/types';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import {
  mockAppState,
  mockEnvVars,
} from '../../../../../../.storybook/mockAppState';
import LegacyDataSidebar from '.';

const baseUrl = 'http://localhost:8080';

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          { table: { schema: 'public', name: 'users' } },
          { table: { schema: 'public', name: 'orders' } },
          { table: { schema: 'public', name: 'order_status' }, is_enum: true },
          { table: { schema: 'audit', name: 'events' } },
        ] as MetadataTable[],
        functions: [{ function: { schema: 'public', name: 'search_users' } }],
        configuration: {
          connection_info: { database_url: { from_env: 'PG_DATABASE_URL' } },
        },
      },
      {
        name: 'chinook',
        kind: 'sqlite',
        tables: [
          { table: ['Album'] },
          { table: ['Artist'] },
        ] as MetadataTable[],
        configuration: {},
      },
    ],
  },
};

const emptyMetadata: Metadata = {
  resource_version: 1,
  metadata: { version: 3, sources: [] },
};

const metadataHandlers = (
  response: Metadata,
  latency: number | 'infinite' = 100,
) => [
  http.post(`${baseUrl}/v1/metadata`, async () => {
    await delay(latency);
    return HttpResponse.json(response);
  }),
];

// The global MemoryRouter starts at "/", so move to the given path before the
// story renders, and expose the `:source` param the tree reads.
const withLocation = (path: string): Decorator => {
  const Navigate = ({ children }: { children: React.ReactNode }) => {
    const navigate = useNavigate();
    const [ready, setReady] = useState(false);
    useLayoutEffect(() => {
      void navigate(path, { replace: true });
      setReady(true);
    }, [navigate]);
    return ready ? children : null;
  };

  return (Story) => (
    <Navigate>
      <Routes>
        <Route path="/data/manage/source/:source/*" element={<Story />} />
        <Route path="*" element={<Story />} />
      </Routes>
    </Navigate>
  );
};

const meta: Meta<typeof LegacyDataSidebar> = {
  component: LegacyDataSidebar,
  title: 'Data/Components/LegacyDataSidebar',
  decorators: [
    (Story) => (
      <div style={{ width: 280 }}>
        <Story />
      </div>
    ),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: metadataHandlers(metadata),
  },
};

export default meta;

type Story = StoryObj<typeof LegacyDataSidebar>;

export const Default: Story = {
  decorators: [withLocation('/data/manage')],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText('Databases (2)')).toBeVisible();
    await expect(canvas.getByText('default')).toBeVisible();
    await expect(canvas.getByText('chinook')).toBeVisible();
    await expect(canvas.getByText('SQL')).toBeVisible();
    await expect(canvas.queryByText('Migrations')).not.toBeInTheDocument();
  },
};

const usersTablePath = `/data/manage/source/default/table?table=${encodeURIComponent(
  JSON.stringify({ schema: 'public', name: 'users' }),
)}`;

export const ActiveSource: Story = {
  decorators: [withLocation(usersTablePath)],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText('users')).toBeVisible();
    await expect(canvas.getByText('orders')).toBeVisible();
    await expect(canvas.getByText('search_users')).toBeVisible();
  },
};

const functionPath = (name: string) =>
  `/data/manage/source/default/function?function=${encodeURIComponent(
    JSON.stringify({ schema: 'public', name }),
  )}`;

const linkAccent = (element: HTMLElement) =>
  element.closest('a')?.getAttribute('data-accent-color');

export const ActiveFunction: Story = {
  decorators: [withLocation(functionPath('search_users'))],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(linkAccent(await canvas.findByText('search_users'))).toBe(
      'indigo',
    );
    await expect(linkAccent(canvas.getByText('users'))).toBe('gray');
  },
};

// A function named like a table in the same schema: only the function is active.
export const FunctionSharingTableName: Story = {
  decorators: [withLocation(functionPath('users'))],
  parameters: {
    msw: metadataHandlers({
      ...metadata,
      metadata: {
        ...metadata.metadata,
        sources: metadata.metadata.sources.map((source) =>
          source.name === 'default'
            ? {
                ...source,
                functions: [
                  ...(source.functions ?? []),
                  { function: { schema: 'public', name: 'users' } },
                ],
              }
            : source,
        ),
      },
    }),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const [table, fn] = await canvas.findAllByText('users');
    await expect(linkAccent(table)).toBe('gray');
    await expect(linkAccent(fn)).toBe('indigo');
  },
};

export const SearchTables: Story = {
  decorators: [withLocation(usersTablePath)],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await canvas.findByText('users');
    await userEvent.type(canvas.getByPlaceholderText('search tables'), 'order');
    await expect(canvas.getByText('orders')).toBeVisible();
    await expect(canvas.getByText('order_status')).toBeVisible();
    await expect(canvas.queryByText('users')).not.toBeInTheDocument();
  },
};

export const NoSources: Story = {
  decorators: [withLocation('/data/manage')],
  parameters: {
    msw: metadataHandlers(emptyMetadata),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText('Databases (0)')).toBeVisible();
    await expect(canvas.getByText('No data available')).toBeVisible();
    await expect(canvas.queryByText('SQL')).not.toBeInTheDocument();
  },
};

export const Loading: Story = {
  decorators: [withLocation('/data/manage')],
  parameters: {
    msw: metadataHandlers(metadata, 'infinite'),
  },
};

export const CliMode: Story = {
  decorators: [
    withLocation('/data/migrations'),
    (Story) => (
      <AppContext.Provider
        value={{
          ...mockAppState,
          envVars: {
            ...mockEnvVars,
            consoleMode: 'cli',
            adminSecret: '',
            apiHost: 'http://localhost',
            apiPort: '9693',
            cliUUID: '00000000-0000-0000-0000-000000000000',
            dataApiUrl: baseUrl,
          },
        }}
      >
        <Story />
      </AppContext.Provider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText('Migrations')).toBeVisible();
  },
};
