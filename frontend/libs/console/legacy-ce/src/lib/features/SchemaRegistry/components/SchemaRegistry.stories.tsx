import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, fn, userEvent, within } from 'storybook/test';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { SchemaDetailsPanel } from './SchemaChangeDetails';
import { SchemaChangeList, SchemaRegistryLayout } from './SchemaRegistryHome';
import { SchemaRegistryHeader } from './Container';
import {
  sampleHistory,
  sampleSchema,
  schemaWithChanges,
} from './SchemaRegistry.fixtures';

const meta = {
  title: 'Features/Schema Registry',
  component: SchemaDetailsPanel,
  decorators: [
    (Story) => (
      <main>
        <Story />
      </main>
    ),
    ReactQueryDecorator(),
  ],
  parameters: { layout: 'padded' },
  args: { schema: sampleSchema },
} satisfies Meta<typeof SchemaDetailsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

const Screen: typeof SchemaDetailsPanel = ({ schema }) => {
  const [page, setPage] = useState(0);
  return (
    <div className="p-4">
      <SchemaRegistryHeader />
      <SchemaRegistryLayout
        sidebar={
          <SchemaChangeList
            schemas={sampleHistory}
            urlSchemaCard={[]}
            pageNumber={page}
            totalCount={25}
            selectedRoleID="admin-schema"
            handlePrev={() => setPage((value) => value - 1)}
            handleNext={(offset) => setPage((value) => value + offset)}
          />
        }
      >
        <SchemaDetailsPanel schema={schema} />
      </SchemaRegistryLayout>
    </div>
  );
};

export const PublishedSchema: Story = {
  render: (args) => <Screen {...args} />,
};

export const NoChanges: Story = {
  args: { schema: schemaWithChanges([]) },
  render: (args) => <Screen {...args} />,
  play: async ({ canvasElement }) => {
    await expect(
      within(canvasElement).getByText('No changes detected'),
    ).toBeVisible();
  },
};

export const ComparisonUnavailable: Story = {
  args: { schema: schemaWithChanges(undefined) },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('Comparison unavailable')).toBeVisible();
    await expect(canvas.getAllByText('Unavailable')).toHaveLength(4);
  },
};

export const FilterAndSearch: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.selectOptions(
      canvas.getByLabelText('Change severity'),
      'BREAKING',
    );
    await expect(canvas.getByText('Showing 1 of 3 changes')).toBeVisible();
    await userEvent.selectOptions(
      canvas.getByLabelText('Change severity'),
      'all',
    );
    await userEvent.type(
      canvas.getByLabelText('Search changes'),
      'display_name',
    );
    await expect(canvas.getByText('Showing 1 of 3 changes')).toBeVisible();
    await userEvent.click(
      canvas.getByRole('button', { name: 'Clear filters' }),
    );
    await expect(canvas.getByText('Showing 3 of 3 changes')).toBeVisible();
  },
};

export const NoMatchingChanges: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.type(
      canvas.getByLabelText('Search changes'),
      'not-a-field',
    );
    await expect(canvas.getByText('No matching changes')).toBeVisible();
    await userEvent.click(
      canvas.getByRole('button', { name: 'Clear filters' }),
    );
    await expect(canvas.getByText('Showing 3 of 3 changes')).toBeVisible();
  },
};

export const NarrowLayout: Story = {
  args: {
    schema: {
      ...sampleSchema,
      roleBasedSchemas: [
        {
          ...sampleSchema.roleBasedSchemas[0],
          role: 'customer_support_readonly_with_a_very_long_role_name',
          hash: sampleSchema.hash.repeat(3),
        },
      ],
    },
  },
  render: (args) => (
    <div style={{ width: 360, maxWidth: '100%' }}>
      <Screen {...args} />
    </div>
  ),
};

export const DarkMode: Story = {
  render: (args) => <Screen {...args} />,
  globals: { appearance: 'dark' },
};

export const EmptyHistory: Story = {
  render: () => (
    <SchemaChangeList
      schemas={[]}
      urlSchemaCard={[]}
      pageNumber={0}
      totalCount={0}
      selectedRoleID={null}
      handlePrev={fn()}
      handleNext={fn()}
    />
  ),
};

export const Pagination: Story = {
  render: (args) => <Screen {...args} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole('button', { name: 'Prev' })).toBeDisabled();
    await userEvent.click(canvas.getByRole('button', { name: 'Next' }));
    await expect(canvas.getByText('Page 2 of 3')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Next' }));
    await expect(canvas.getByRole('button', { name: 'Next' })).toBeDisabled();
    await userEvent.click(canvas.getByRole('button', { name: 'Prev' }));
    await expect(canvas.getByText('Page 2 of 3')).toBeVisible();
  },
};

export const GraphQLSchema: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('tab', { name: 'GraphQL' }));
    await expect(canvas.getByRole('tab', { name: 'GraphQL' })).toHaveAttribute(
      'aria-selected',
      'true',
    );
    await expect(
      canvas.getByText('display_name', { exact: true }),
    ).toBeVisible();
  },
};
