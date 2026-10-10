import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { Source } from '@hasura/shared/types';
import { AddTableForm } from './AddTableForm';

// jsdom does not implement ResizeObserver, which Radix Select relies on.
class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

const mutate = vi.fn();

vi.mock('@hasura/metadata/data-source', () => ({
  useCreateTable: () => ({ mutate, isPending: false }),
  useSupportedDataTypes: () => ({
    data: { text: ['text'], integer: ['integer'], boolean: ['boolean'] },
  }),
  useGetTableSchemas: () => ({ data: ['public'] }),
  useTableColumns: () => ({ data: { columns: [] } }),
  getDatabaseMethods: () => ({
    config: { getViolationActions: () => ['restrict', 'cascade'] },
  }),
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({ data: [] }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  MetadataSelectors: { getTables: () => (m: unknown) => m },
}));

const source = {
  name: 'default',
  kind: 'postgres',
  tables: [],
} as unknown as Source;

const setup = () => {
  const onCreated = vi.fn();
  render(
    <Theme>
      <AddTableForm
        source={source}
        targetSchema="public"
        onCreated={onCreated}
      />
    </Theme>,
  );
  return { onCreated };
};

describe('AddTableForm', () => {
  beforeEach(() => mutate.mockClear());

  it('renders the core sections with one initial column row and constraint sections', () => {
    setup();
    expect(
      screen.getByRole('button', { name: /add table/i }),
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /add column/i }),
    ).toBeInTheDocument();
    expect(screen.getAllByPlaceholderText('column_name')).toHaveLength(1);
    // new sections/toggles
    expect(
      screen.getByRole('button', { name: /add foreign key/i }),
    ).toBeInTheDocument();
    expect(screen.getByText('Array')).toBeInTheDocument();
    expect(screen.getByText('Unique')).toBeInTheDocument();
  });

  it('adds and removes column rows', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /add column/i }));
    expect(screen.getAllByPlaceholderText('column_name')).toHaveLength(2);

    await userEvent.click(
      screen.getByRole('button', { name: /remove column 2/i }),
    );
    expect(screen.getAllByPlaceholderText('column_name')).toHaveLength(1);
  });

  it('disables removing the last remaining column', () => {
    setup();
    expect(
      screen.getByRole('button', { name: /remove column 1/i }),
    ).toBeDisabled();
  });

  it('adds a check-constraint row on demand', async () => {
    setup();
    expect(
      screen.queryByPlaceholderText('positive_total'),
    ).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByRole('button', { name: /add check constraint/i }),
    );
    expect(screen.getByPlaceholderText('positive_total')).toBeInTheDocument();
  });

  it('adds a foreign key row with a column mapping and cascade selects', async () => {
    setup();
    expect(screen.queryByText('Reference table')).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByRole('button', { name: /add foreign key/i }),
    );
    // The FK form lives in a DelayedDialog that mounts its content after a tick.
    expect(await screen.findByText('Reference table')).toBeInTheDocument();
    expect(screen.getByText('On update')).toBeInTheDocument();
    expect(screen.getByText('On delete')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /add column mapping/i }),
    ).toBeInTheDocument();
  });

  it('blocks submission and does not create a table when required fields are missing', async () => {
    setup();
    // No table name, no primary key selected -> zod validation should block.
    await userEvent.click(screen.getByRole('button', { name: /add table/i }));

    expect(
      await screen.findByText(/table name is required/i),
    ).toBeInTheDocument();
    expect(mutate).not.toHaveBeenCalled();
  });
});
