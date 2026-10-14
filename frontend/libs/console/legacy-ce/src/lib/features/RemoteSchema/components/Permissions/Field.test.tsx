import { render, screen, waitFor } from '@testing-library/react';
import { Field } from './Field';
import { PermissionEditorContext } from './context';
import type { FieldType, ArgTreeType } from './types';

// MOCKS (labelled): child widgets are stubbed to keep the test focused on the
// preset-mode seeding + the argTree sync effect that this component owns.
vi.mock('./ArgSelect', () => ({
  ArgSelect: () => <div data-testid="arg-select" />,
}));
vi.mock('./CollapsedField', () => ({
  CollapsedField: () => <div data-testid="collapsed-field" />,
}));
vi.mock('./Pen', () => ({ default: () => <span>pen</span> }));
vi.mock('./utils', () => ({
  getChildArguments: () => undefined,
  generateTypeString: () => '',
}));

const buildField = (): FieldType =>
  ({
    name: 'myField',
    checked: true,
    isInputObjectType: true,
    return: '',
    args: { myField: {}, arg2: {} },
  }) as unknown as FieldType;

const renderField = (argTree: ArgTreeType, setArgTree = vi.fn()) =>
  render(
    <PermissionEditorContext.Provider
      value={{ argTree, setArgTree, scrollToElement: vi.fn() }}
    >
      <Field i={buildField()} setItem={vi.fn()} expanded />
    </PermissionEditorContext.Provider>,
  );

describe('Field (preset-mode lazy init + argTree sync)', () => {
  it('shows the pen (no auto-expand) when there are no prefilled presets', () => {
    renderField({});
    expect(screen.getByText('pen')).toBeInTheDocument();
    expect(screen.queryByTestId('arg-select')).not.toBeInTheDocument();
  });

  it('auto-expands into preset mode on mount when the argTree has prefilled values', () => {
    renderField({ myField: { someArg: 'x' } });
    // Preset mode seeded via lazy init -> args render and the pen is hidden.
    expect(screen.getAllByTestId('arg-select').length).toBeGreaterThan(0);
    expect(screen.queryByText('pen')).not.toBeInTheDocument();
  });

  it('syncs the prefilled fieldVal into the context argTree exactly once (no loop)', async () => {
    const setArgTree = vi.fn();
    renderField({ myField: { someArg: 'x' } }, setArgTree);

    await waitFor(() => expect(setArgTree).toHaveBeenCalledTimes(1));

    // Verify the updater writes the field's value into the tree.
    const updater = setArgTree.mock.calls[0][0] as (
      t: ArgTreeType,
    ) => ArgTreeType;
    expect(updater({})).toEqual({ myField: { someArg: 'x' } });

    // No runaway: fieldVal is stable so the effect must not fire again.
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(setArgTree).toHaveBeenCalledTimes(1);
  });
});
