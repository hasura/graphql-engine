import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { CheckConstraints } from './CheckConstraints';

const createCheckConstraint = vi.fn();
const dropCheckConstraint = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);

let listSupported = true;
let addSupported = true;
let dropSupported = true;
let existing = [{ name: 'positive_total', check: 'CHECK ((total > 0))' }];

vi.mock('@hasura/metadata/data-source', () => ({
  useCreateCheckConstraint: () => ({
    mutate: createCheckConstraint,
    isPending: false,
  }),
  useDropCheckConstraint: () => ({ mutateAsync: dropCheckConstraint }),
  useTableCheckConstraints: () => ({
    data: existing,
    isLoading: false,
    isError: false,
  }),
  getDatabaseMethods: () => ({
    introspection: {
      ...(listSupported ? { getCheckConstraints: () => undefined } : {}),
    },
    modify: {
      ...(addSupported ? { createCheckConstraint: () => undefined } : {}),
      ...(dropSupported ? { dropCheckConstraint: () => undefined } : {}),
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  const React = await import('react');
  const { Controller } = await import('react-hook-form');
  return {
    ...actual,
    useDestructiveConfirm: () => confirmSpy,
    // react-ace (behind `CodeEditorField`) renders its placeholder as a decorative
    // `.ace_placeholder` div and exposes no typeable input under jsdom. Render the
    // SQL "Check expression" editor as a plain RHF-controlled <textarea> keeping
    // the same name/placeholder so the field stays drivable and wired to the form.
    CodeEditorField: ({
      name,
      placeholder,
      label,
    }: {
      name: string;
      placeholder?: string;
      label?: string;
    }) =>
      React.createElement(Controller, {
        name,
        render: ({
          field,
        }: {
          field: { value?: string; onChange: (value: string) => void };
        }) =>
          React.createElement('textarea', {
            'aria-label': label,
            placeholder,
            value: field.value ?? '',
            onChange: (e: React.ChangeEvent<HTMLTextAreaElement>) =>
              field.onChange(e.target.value),
          }),
      }),
  };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(
    <Theme>
      <CheckConstraints source={source} table={table} isView={false} />
    </Theme>,
  );

describe('ModifyTable CheckConstraints', () => {
  beforeEach(() => {
    createCheckConstraint.mockClear();
    dropCheckConstraint.mockClear();
    confirmSpy.mockClear();
    listSupported = true;
    addSupported = true;
    dropSupported = true;
    existing = [{ name: 'positive_total', check: 'CHECK ((total > 0))' }];
  });

  it('lists existing check constraints and the add button', () => {
    setup();
    expect(screen.getByText('positive_total')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /add check constraint/i }),
    ).toBeInTheDocument();
  });

  it('removes a check constraint through the confirmation flow', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /remove/i }));
    expect(confirmSpy).toHaveBeenCalledTimes(1);
    await waitFor(() =>
      expect(dropCheckConstraint).toHaveBeenCalledWith(
        expect.objectContaining({
          source: { name: 'default', kind: 'postgres' },
          table: table.table,
          constraintName: 'positive_total',
        }),
      ),
    );
  });

  it('creates a check constraint from the add dialog', async () => {
    existing = [];
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /add check constraint/i }),
    );
    await userEvent.type(
      await screen.findByPlaceholderText('positive_total'),
      'nonneg',
    );
    await userEvent.type(screen.getByPlaceholderText('total > 0'), 'qty >= 0');
    await userEvent.click(screen.getByRole('button', { name: /^add$/i }));
    await waitFor(() =>
      expect(createCheckConstraint).toHaveBeenCalledWith(
        expect.objectContaining({
          constraintName: 'nonneg',
          check: 'qty >= 0',
          table: table.table,
        }),
      ),
    );
  });

  it('hides the remove button when the driver lacks dropCheckConstraint', () => {
    dropSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /remove/i }),
    ).not.toBeInTheDocument();
  });

  it('hides the add button when the driver lacks createCheckConstraint', () => {
    addSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /add check constraint/i }),
    ).not.toBeInTheDocument();
  });
});
