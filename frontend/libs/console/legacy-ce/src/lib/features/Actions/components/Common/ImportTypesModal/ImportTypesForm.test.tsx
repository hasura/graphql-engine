import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { buildClientSchema } from 'graphql';
import { ImportTypesForm } from './ImportTypesForm';
import { schema } from './__tests__/mockSchema';

// Stub the Ace editor (it only displays the derived typeDef).
vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return {
    ...actual,
    AceEditor: ({ value }: { value?: string }) => (
      <textarea data-testid="type-def" readOnly value={value} />
    ),
  };
});

const clientSchema = buildClientSchema(schema as any);

vi.mock('@hasura/metadata/api', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return {
    ...actual,
    useIntrospectSchema: () => ({ data: clientSchema, isLoading: false }),
  };
});

const renderForm = () => {
  const setValues = vi.fn();
  render(
    <Theme>
      <ImportTypesForm setValues={setValues} />
    </Theme>,
  );
  return { setValues };
};

describe('ImportTypesForm', () => {
  it('reports the initial (empty) selection on mount with a derived typeDef', async () => {
    const { setValues } = renderForm();

    await waitFor(() => {
      expect(setValues).toHaveBeenCalled();
    });

    const firstCall = setValues.mock.calls[0][0];
    expect(firstCall.selectedTypes).toEqual([]);
    expect(firstCall.typeDef).toContain('Imported Types from table schema');
  });

  it('derives the typeDef and reports the selection when a type is toggled', async () => {
    const { setValues } = renderForm();

    const checkboxes = await screen.findAllByRole('checkbox');
    expect(checkboxes.length).toBeGreaterThan(0);

    setValues.mockClear();
    fireEvent.click(checkboxes[0]);

    await waitFor(() => {
      expect(setValues).toHaveBeenCalled();
    });

    const calls = setValues.mock.calls;
    const lastCall = calls[calls.length - 1]?.[0];
    expect(lastCall.selectedTypes).toHaveLength(1);
    expect(typeof lastCall.typeDef).toBe('string');
  });

  it('does not re-report in a runaway loop after a toggle', async () => {
    const { setValues } = renderForm();

    const checkboxes = await screen.findAllByRole('checkbox');
    setValues.mockClear();
    fireEvent.click(checkboxes[0]);

    await waitFor(() => {
      expect(setValues).toHaveBeenCalled();
    });

    const countAfterSettle = setValues.mock.calls.length;
    await new Promise((r) => setTimeout(r, 50));
    expect(setValues.mock.calls.length).toBe(countAfterSettle);
  });
});
