import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { GraphQLSchema } from 'graphql';
import PermissionEditor from './PermissionEditor';
import type { FieldType, RemoteSchemaFields } from './types';
import type { PermissionEdit } from '../../hooks/useRemoteSchemaRelationshipForm';

// jsdom lacks ResizeObserver, used by the Radix-based search Input.
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

// MOCKS (labelled): `Tree` is stubbed to a button that checks every field (so a
// test can simulate user selection); `generateSDL` reflects whether any field is
// checked so we can assert the derived SDL / save-enabled behaviour.
const generateSDLSpy = vi.fn((state: RemoteSchemaFields[] | FieldType[]) =>
  (state as FieldType[]).some((f) => f.checked) ? 'GENERATED_SDL' : '',
);

vi.mock('./Tree', () => ({
  default: ({
    list,
    setState,
  }: {
    list: FieldType[];
    setState: (d: FieldType[]) => void;
  }) => (
    <button
      type="button"
      onClick={() => setState(list.map((f) => ({ ...f, checked: true })))}
    >
      check-all
    </button>
  ),
}));

vi.mock('./utils', () => ({
  generateSDL: (...args: unknown[]) =>
    generateSDLSpy(...(args as [RemoteSchemaFields[] | FieldType[]])),
  getArgTreeFromPermissionSDL: () => ({}),
}));

const permissionEdit: PermissionEdit = {
  newRole: '',
  isNewRole: true,
  isNewPerm: true,
  role: 'user',
};

const fields = (names: string[]): RemoteSchemaFields[] =>
  names.map((name) => ({ name, checked: false }) as unknown as FieldType);

const baseProps = {
  permissionEdit,
  isEditing: true,
  isFetching: false,
  schemaDefinition: '',
  introspectionSchema: {} as GraphQLSchema,
  setSchemaDefinition: vi.fn(),
  permCloseEdit: vi.fn(),
  saveRemoteSchemaPermission: vi.fn(),
  removeRemoteSchemaPermission: vi.fn(),
};

const saveButton = () =>
  screen.getByRole('button', {
    name: 'Save Permissions',
  }) as HTMLButtonElement;

beforeEach(() => {
  generateSDLSpy.mockClear();
});

describe('PermissionEditor (derived resultString + prop-reset)', () => {
  it('disables Save when the derived SDL is empty (nothing checked)', () => {
    render(
      <PermissionEditor
        {...baseProps}
        remoteSchemaFields={fields(['TypeA', 'TypeB'])}
      />,
    );
    expect(saveButton()).toBeDisabled();
  });

  it('enables Save once fields are checked (SDL derived from state)', async () => {
    const user = userEvent.setup();
    render(
      <PermissionEditor
        {...baseProps}
        remoteSchemaFields={fields(['TypeA', 'TypeB'])}
      />,
    );
    expect(saveButton()).toBeDisabled();

    await user.click(screen.getByRole('button', { name: 'check-all' }));
    await waitFor(() => expect(saveButton()).toBeEnabled());
  });

  it('resets the editable state when the remoteSchemaFields prop changes', async () => {
    const user = userEvent.setup();
    const { rerender } = render(
      <PermissionEditor
        {...baseProps}
        remoteSchemaFields={fields(['TypeA'])}
      />,
    );
    await user.click(screen.getByRole('button', { name: 'check-all' }));
    await waitFor(() => expect(saveButton()).toBeEnabled());

    // New (fresh, all-unchecked) field set -> state resets -> Save disabled.
    rerender(
      <PermissionEditor
        {...baseProps}
        remoteSchemaFields={fields(['TypeC'])}
      />,
    );
    await waitFor(() => expect(saveButton()).toBeDisabled());
  });

  it('does not recompute the SDL in a render loop', async () => {
    render(
      <PermissionEditor
        {...baseProps}
        remoteSchemaFields={fields(['TypeA', 'TypeB'])}
      />,
    );
    await new Promise((resolve) => setTimeout(resolve, 50));
    // useMemo keeps generateSDL bounded despite the search input / renders.
    expect(generateSDLSpy.mock.calls.length).toBeLessThan(4);
  });
});
