import { render, screen } from '@testing-library/react';
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
    // create mode starts with no selected options
    expect(
      screen.getByText('No singular/Non-inherited Roles available'),
    ).toBeInTheDocument();
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
