import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { MetadataTable, Source } from '@hasura/shared/types';
import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';

// `useScrollIntoView` reaches into refs/layout which jsdom doesn't implement.
vi.mock('@hasura/shared/hooks', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return { ...actual, useScrollIntoView: vi.fn() };
});

// Drive `data`/`isFetching` from the test so we can simulate the lazy metadata
// cache being present-but-refetching during SPA navigation.
const mockUseMetadata = vi.fn();
vi.mock('@hasura/metadata/api', () => ({
  useMetadata: (...args: unknown[]) => mockUseMetadata(...args),
}));

// The real wrapper mounts the heavy form stack (useConsoleForm, introspection,
// database methods). Stub it and record each mount so we can assert the form is
// (re)mounted exactly when we expect.
const mockWrapperMount = vi.fn();
vi.mock('./components/ModifyPermission', async () => {
  const React = await import('react');
  const MockPermissionsFormWrapper = (props: { roleName: string }) => {
    const counted = React.useRef(false);
    if (!counted.current) {
      counted.current = true;
      mockWrapperMount(props.roleName);
    }
    return React.createElement(
      'div',
      { 'data-testid': 'permissions-form-wrapper' },
      `wrapper:${props.roleName}`,
    );
  };
  return { __esModule: true, default: MockPermissionsFormWrapper };
});

vi.mock('./components', () => ({
  ClonePermissionsSection: () => null,
}));

const tableIdentity = { schema: 'public', name: 'games' };

const metadataResult = { metadata: { version: 3, sources: [] } };

const makeTable = (roles: string[]): MetadataTable =>
  ({
    table: tableIdentity,
    select_permissions: roles.map((role) => ({
      role,
      permission: { columns: ['id'], filter: {} },
    })),
  }) as unknown as MetadataTable;

const source = {
  name: 'default',
  kind: 'postgres',
  tables: [],
} as unknown as Source;

const baseProps = (
  overrides: Partial<PermissionsFormProps> = {},
): PermissionsFormProps => ({
  source,
  table: makeTable([]),
  queryType: 'select',
  roleName: 'user',
  accessType: 'fullAccess',
  handleClose: vi.fn(),
  ...overrides,
});

const renderForm = (props: PermissionsFormProps) =>
  render(
    <Theme>
      <PermissionsForm {...props} />
    </Theme>,
  );

beforeEach(() => {
  mockWrapperMount.mockClear();
  mockUseMetadata.mockReset();
});

describe('PermissionsForm async hydration (issue #10907)', () => {
  it('waits for the permission to hydrate before mounting the form', () => {
    // Metadata is cached (truthy) but still refetching, and the role's
    // permission is not yet present on the table snapshot.
    mockUseMetadata.mockReturnValue({
      data: metadataResult,
      isFetching: true,
    });

    const { rerender } = renderForm(baseProps({ table: makeTable([]) }));

    // The form must NOT mount against the incomplete snapshot (that is what
    // produced the blank Row radio / Column checkboxes).
    expect(
      screen.queryByTestId('permissions-form-wrapper'),
    ).not.toBeInTheDocument();
    expect(mockWrapperMount).not.toHaveBeenCalled();

    // Refetch resolves with the permission present.
    mockUseMetadata.mockReturnValue({
      data: metadataResult,
      isFetching: false,
    });
    rerender(
      <Theme>
        <PermissionsForm {...baseProps({ table: makeTable(['user']) })} />
      </Theme>,
    );

    expect(screen.getByTestId('permissions-form-wrapper')).toHaveTextContent(
      'wrapper:user',
    );
    expect(mockWrapperMount).toHaveBeenCalledTimes(1);
  });

  it('does not remount the form on a background refetch, preserving edits', () => {
    mockUseMetadata.mockReturnValue({
      data: metadataResult,
      isFetching: false,
    });
    const props = baseProps({ table: makeTable(['user']) });
    const { rerender } = renderForm(props);

    expect(mockWrapperMount).toHaveBeenCalledTimes(1);

    // A background refetch flips isFetching on and off with the same identity.
    mockUseMetadata.mockReturnValue({ data: metadataResult, isFetching: true });
    rerender(
      <Theme>
        <PermissionsForm {...props} />
      </Theme>,
    );
    mockUseMetadata.mockReturnValue({
      data: metadataResult,
      isFetching: false,
    });
    rerender(
      <Theme>
        <PermissionsForm {...props} />
      </Theme>,
    );

    // Still a single mount: the form keeps the user's in-progress edits.
    expect(mockWrapperMount).toHaveBeenCalledTimes(1);
  });

  it('remounts the form when navigating to a different role', () => {
    mockUseMetadata.mockReturnValue({
      data: metadataResult,
      isFetching: false,
    });
    const { rerender } = renderForm(
      baseProps({ table: makeTable(['user', 'manager']), roleName: 'user' }),
    );

    expect(mockWrapperMount).toHaveBeenLastCalledWith('user');
    expect(mockWrapperMount).toHaveBeenCalledTimes(1);

    rerender(
      <Theme>
        <PermissionsForm
          {...baseProps({
            table: makeTable(['user', 'manager']),
            roleName: 'manager',
          })}
        />
      </Theme>,
    );

    // The permissions-table machine keeps the form open across role switches,
    // so without the identity key the form would keep role `user`'s values.
    expect(mockWrapperMount).toHaveBeenCalledTimes(2);
    expect(mockWrapperMount).toHaveBeenLastCalledWith('manager');
  });
});
