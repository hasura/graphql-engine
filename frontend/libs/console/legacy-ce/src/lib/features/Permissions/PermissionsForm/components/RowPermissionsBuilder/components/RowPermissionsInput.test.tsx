import { fireEvent, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { RowPermissionsInput } from './RowPermissionsInput';

// Ace doesn't run in jsdom: a textarea that shows the gutter annotations
vi.mock('react-ace', async (importActual) => ({
  // the real module loads the `ace` global the editor extensions need
  ...(await importActual<object>()),
  default: ({
    value,
    onChange,
    annotations,
  }: {
    value?: string;
    onChange?: (value: string) => void;
    annotations?: { row: number; text: string }[];
  }) => (
    <textarea
      data-testid="json-editor"
      data-annotations={JSON.stringify(annotations ?? [])}
      value={value}
      onChange={(event) => onChange?.(event.target.value)}
    />
  ),
}));

const permissions = { id: { _eq: 'X-Hasura-User-Id' } };

const setup = () => {
  const onPermissionsChange = vi.fn();
  render(
    <Theme>
      <RowPermissionsInput
        permissions={permissions}
        table={{ schema: 'public', name: 'users' }}
        tables={[]}
        logicalModel={undefined}
        logicalModels={[]}
        comparators={{}}
        onPermissionsChange={onPermissionsChange}
      />
    </Theme>,
  );
  return { onPermissionsChange };
};

const editor = () => screen.getByTestId('json-editor') as HTMLTextAreaElement;

describe('RowPermissionsInput', () => {
  it('shows the builder by default', () => {
    setup();

    expect(screen.getByTestId('RootInputReady')).toBeInTheDocument();
    expect(screen.queryByTestId('json-editor')).not.toBeInTheDocument();
  });

  it('switches between the builder and the JSON editor', async () => {
    setup();

    await userEvent.click(screen.getByRole('radio', { name: 'JSON' }));

    expect(screen.queryByTestId('RootInputReady')).not.toBeInTheDocument();
    expect(editor().value).toBe(JSON.stringify(permissions, null, 2));

    await userEvent.click(screen.getByRole('radio', { name: 'Builder' }));

    expect(screen.getByTestId('RootInputReady')).toBeInTheDocument();
    expect(screen.queryByTestId('json-editor')).not.toBeInTheDocument();
  });

  it('applies valid JSON', async () => {
    const { onPermissionsChange } = setup();
    await userEvent.click(screen.getByRole('radio', { name: 'JSON' }));

    fireEvent.change(editor(), { target: { value: '{"name":{"_eq":"a"}}' } });

    expect(onPermissionsChange).toHaveBeenLastCalledWith({
      name: { _eq: 'a' },
    });
    // the typed text is kept as is
    expect(editor().value).toBe('{"name":{"_eq":"a"}}');
    expect(JSON.parse(editor().dataset.annotations ?? '')).toEqual([]);
  });

  it('keeps invalid JSON and marks the error without applying it', async () => {
    const { onPermissionsChange } = setup();
    await userEvent.click(screen.getByRole('radio', { name: 'JSON' }));

    fireEvent.change(editor(), { target: { value: '{\n  "id" 1\n}' } });

    expect(onPermissionsChange).not.toHaveBeenCalled();
    expect(editor().value).toBe('{\n  "id" 1\n}');
    const [annotation] = JSON.parse(editor().dataset.annotations ?? '');
    expect(annotation).toMatchObject({ row: 1, type: 'error' });
  });
});
