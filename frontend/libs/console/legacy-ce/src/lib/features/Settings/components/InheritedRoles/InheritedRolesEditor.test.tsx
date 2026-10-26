import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { vi } from 'vitest';
import InheritedRolesEditor, { EditorProps } from './InheritedRolesEditor';

const baseProps = (overrides: Partial<EditorProps> = {}): EditorProps => ({
  allRoles: ['a', 'b', 'c'],
  inheritedRole: null,
  inheritedRoleName: 'newRole',
  onSave: vi.fn().mockResolvedValue(undefined),
  isCollapsed: false,
  cancelCb: vi.fn(),
  ...overrides,
});

describe('InheritedRolesEditor prop sync (adjust-state-during-render)', () => {
  it('renders nothing while collapsed', () => {
    render(<InheritedRolesEditor {...baseProps({ isCollapsed: true })} />);
    expect(screen.queryByText('Save Role')).not.toBeInTheDocument();
  });

  it('renders create mode with the provided role name on initial render', () => {
    render(<InheritedRolesEditor {...baseProps()} />);
    expect(screen.getByText('Create Role:')).toBeInTheDocument();
    expect(screen.getByText('newRole')).toBeInTheDocument();
    // create mode starts with no selected options, but every role to pick from
    expect(
      screen.queryByText('No singular/Non-inherited Roles available'),
    ).not.toBeInTheDocument();
    ['a', 'b', 'c'].forEach((role) =>
      expect(screen.getByRole('checkbox', { name: role })).not.toBeChecked(),
    );
    expect(screen.getByRole('button', { name: 'Save Role' })).toBeDisabled();
  });

  it('shows the empty state only when there is no role to pick', () => {
    render(<InheritedRolesEditor {...baseProps({ allRoles: [] })} />);
    expect(
      screen.getByText('No singular/Non-inherited Roles available'),
    ).toBeInTheDocument();
  });

  it('filters the roles to pick, keeping the hidden ones selected', async () => {
    const user = userEvent.setup();
    const onSave = vi.fn().mockResolvedValue(undefined);
    render(
      <InheritedRolesEditor
        {...baseProps({ allRoles: ['admin', 'editor', 'user'], onSave })}
      />,
    );

    await user.click(screen.getByRole('checkbox', { name: 'admin' }));
    await user.type(screen.getByPlaceholderText('Filter Roles...'), 'us');
    expect(
      screen.queryByRole('checkbox', { name: 'admin' }),
    ).not.toBeInTheDocument();
    await user.click(screen.getByRole('checkbox', { name: 'user' }));

    await user.click(screen.getByRole('button', { name: 'Save Role' }));
    expect(onSave).toHaveBeenCalledWith({
      role_name: 'newRole',
      role_set: ['admin', 'user'],
    });
  });

  it('selects the role set of the role switched to', () => {
    const { rerender } = render(
      <InheritedRolesEditor
        {...baseProps({
          inheritedRole: { role_name: 'x', role_set: ['a'] },
          inheritedRoleName: undefined,
        })}
      />,
    );
    rerender(
      <InheritedRolesEditor
        {...baseProps({
          inheritedRole: { role_name: 'y', role_set: ['b', 'c'] },
          inheritedRoleName: undefined,
        })}
      />,
    );

    expect(screen.getByRole('checkbox', { name: 'a' })).not.toBeChecked();
    expect(screen.getByRole('checkbox', { name: 'b' })).toBeChecked();
    expect(screen.getByRole('checkbox', { name: 'c' })).toBeChecked();
  });

  it('syncs to the new role name when the inheritedRoleName prop changes', () => {
    const { rerender } = render(<InheritedRolesEditor {...baseProps()} />);
    expect(screen.getByText('newRole')).toBeInTheDocument();

    rerender(
      <InheritedRolesEditor
        {...baseProps({ inheritedRoleName: 'renamedRole' })}
      />,
    );
    expect(screen.getByText('renamedRole')).toBeInTheDocument();
    expect(screen.queryByText('newRole')).not.toBeInTheDocument();
  });

  it('derives selected options from the role set in edit mode', () => {
    render(
      <InheritedRolesEditor
        {...baseProps({
          inheritedRole: { role_name: 'x', role_set: ['a', 'b'] },
          inheritedRoleName: undefined,
        })}
      />,
    );
    expect(screen.getByText('Edit Role:')).toBeInTheDocument();
    expect(screen.getByText('x')).toBeInTheDocument();
    // options derived (non-empty) -> the empty-state text is not shown
    expect(
      screen.queryByText('No singular/Non-inherited Roles available'),
    ).not.toBeInTheDocument();
  });

  it('switches from create to edit mode when the inheritedRole prop appears without looping', () => {
    const { rerender } = render(<InheritedRolesEditor {...baseProps()} />);
    expect(screen.getByText('Create Role:')).toBeInTheDocument();

    expect(() =>
      rerender(
        <InheritedRolesEditor
          {...baseProps({
            inheritedRole: { role_name: 'x', role_set: ['c'] },
            inheritedRoleName: undefined,
          })}
        />,
      ),
    ).not.toThrow();

    expect(screen.getByText('Edit Role:')).toBeInTheDocument();
    expect(screen.getByText('x')).toBeInTheDocument();
  });
});
