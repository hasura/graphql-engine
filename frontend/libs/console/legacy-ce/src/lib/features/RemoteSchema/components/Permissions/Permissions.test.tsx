import { render, screen, waitFor } from '@testing-library/react';
import Permissions from './Permissions';
import type { RemoteSchema } from '@hasura/shared/types';

// MOCKS (labelled): the whole data/hook layer is stubbed so the test can drive
// the inputs of the remoteSchemaFields sync (schema availability, new-role flag,
// SDL) and observe what PermissionEditor receives.
const mockSchema = { __schema: true } as never;

const getFieldsSpy = vi.fn(() => [{}, {}, {}]);

type FormState = {
  bulkSelect: string[];
  permSetBulkSelect: () => void;
  permissionEdit: {
    isNewRole: boolean;
    role: string;
    newRole: string;
    isNewPerm: boolean;
  };
  permSetRoleName: () => void;
  permOpenEdit: () => void;
  permCloseEdit: () => void;
  isEditing: boolean;
  schemaDefinition: string;
  setSchemaDefinition: () => void;
  setBulkSelect: () => void;
};

let formState: FormState;

vi.mock('@hasura/shared/hooks', () => ({ useDocumentTitle: () => undefined }));
vi.mock('@hasura/shared/context', () => ({
  useAppContext: () => ({ readOnlyMode: false }),
}));
vi.mock('@hasura/metadata/api', () => ({
  useInconsistentMetadata: () => ({ data: undefined }),
  useSaveRemoteSchemaPermission: () => ({ mutate: vi.fn(), isPending: false }),
  useDropRemoteSchemaPermissions: () => ({ mutate: vi.fn(), isPending: false }),
  useDropRemoteSchemaPermissionMultipleRoles: () => ({ mutate: vi.fn() }),
  useIntrospectRemoteSchema: () => ({
    data: mockSchema,
    error: null,
    refetch: vi.fn(),
  }),
}));
vi.mock('@hasura/metadata/helpers', () => ({
  findInconsistentRemoteSchema: () => undefined,
}));
vi.mock('../../hooks/useRemoteSchemaRelationshipForm', () => ({
  default: () => formState,
}));
vi.mock('./utils', () => ({
  getRemoteSchemaFields: () => getFieldsSpy(),
  buildSchemaFromRoleDefn: () => null,
}));
vi.mock('./PermissionsTable', () => ({
  default: () => <div data-testid="perm-table" />,
}));
vi.mock('./PermissionEditor', () => ({
  default: ({ remoteSchemaFields }: { remoteSchemaFields: unknown[] }) => (
    <div data-testid="perm-editor">fields:{remoteSchemaFields.length}</div>
  ),
}));
vi.mock('./BulkSelect', () => ({ default: () => <div /> }));
vi.mock('../InconsistentBadge', () => ({ InconsistentBadge: () => <div /> }));

const buildForm = (overrides: Partial<FormState> = {}): FormState => ({
  bulkSelect: [],
  permSetBulkSelect: vi.fn(),
  permissionEdit: {
    isNewRole: false,
    role: 'user',
    newRole: '',
    isNewPerm: false,
  },
  permSetRoleName: vi.fn(),
  permOpenEdit: vi.fn(),
  permCloseEdit: vi.fn(),
  isEditing: false,
  schemaDefinition: '',
  setSchemaDefinition: vi.fn(),
  setBulkSelect: vi.fn(),
  ...overrides,
});

const currentRemoteSchema = { name: 'my_rs' } as RemoteSchema;

beforeEach(() => {
  getFieldsSpy.mockClear();
});

describe('Permissions (remoteSchemaFields sync during render)', () => {
  it('derives remoteSchemaFields from the schema when it is available', async () => {
    formState = buildForm();
    render(
      <Permissions allRoles={[]} currentRemoteSchema={currentRemoteSchema} />,
    );
    await waitFor(() =>
      expect(screen.getByTestId('perm-editor')).toHaveTextContent('fields:3'),
    );
    expect(getFieldsSpy).toHaveBeenCalled();
  });

  it('does NOT reset remoteSchemaFields for a new role that already has an SDL (persists user work)', async () => {
    formState = buildForm({
      permissionEdit: {
        isNewRole: true,
        role: '',
        newRole: 'newRole',
        isNewPerm: true,
      },
      schemaDefinition: 'type Query { stuff: String }',
    });
    render(
      <Permissions allRoles={[]} currentRemoteSchema={currentRemoteSchema} />,
    );
    await screen.findByTestId('perm-editor');
    // The skip branch keeps the initial empty array.
    expect(screen.getByTestId('perm-editor')).toHaveTextContent('fields:0');
    expect(getFieldsSpy).not.toHaveBeenCalled();
  });

  it('does not re-derive the fields in a render loop', async () => {
    formState = buildForm();
    render(
      <Permissions allRoles={[]} currentRemoteSchema={currentRemoteSchema} />,
    );
    await waitFor(() => expect(getFieldsSpy).toHaveBeenCalled());
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(getFieldsSpy.mock.calls.length).toBeLessThan(3);
  });
});
