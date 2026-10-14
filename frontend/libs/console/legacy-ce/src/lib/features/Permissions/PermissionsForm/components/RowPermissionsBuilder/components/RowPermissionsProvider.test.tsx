import { useContext } from 'react';
import { render, screen } from '@testing-library/react';
import {
  RowPermissionsProvider,
  rowPermissionsContext,
} from './RowPermissionsProvider';

// Regression for the set-state-in-effect -> "adjust state while rendering" fix:
// when the server-provided `permissions` prop changes, the internal state must
// sync to it, without an effect, without calling onPermissionsChange, and
// without a runaway render loop.

const renderSpy = vi.fn();

const Consumer = () => {
  const { permissions } = useContext(rowPermissionsContext);
  renderSpy();
  return <div data-testid="permissions">{JSON.stringify(permissions)}</div>;
};

const setup = (permissions: Record<string, any>, onChange = vi.fn()) => {
  const utils = render(
    <RowPermissionsProvider
      operators={{}}
      comparators={{}}
      permissions={permissions}
      onPermissionsChange={onChange}
    >
      <Consumer />
    </RowPermissionsProvider>,
  );
  return { ...utils, onChange };
};

describe('RowPermissionsProvider', () => {
  beforeEach(() => {
    renderSpy.mockClear();
  });

  it('renders the initial permissions from props', () => {
    setup({ id: { _eq: 1 } });
    expect(screen.getByTestId('permissions')).toHaveTextContent(
      JSON.stringify({ id: { _eq: 1 } }),
    );
  });

  it('syncs internal state when the permissions prop changes', () => {
    const { rerender } = setup({ id: { _eq: 1 } });

    rerender(
      <RowPermissionsProvider
        operators={{}}
        comparators={{}}
        permissions={{ name: { _eq: 'a' } }}
        onPermissionsChange={vi.fn()}
      >
        <Consumer />
      </RowPermissionsProvider>,
    );

    expect(screen.getByTestId('permissions')).toHaveTextContent(
      JSON.stringify({ name: { _eq: 'a' } }),
    );
  });

  it('does not call onPermissionsChange when syncing from the prop', () => {
    const { rerender, onChange } = setup({ id: { _eq: 1 } });
    expect(onChange).not.toHaveBeenCalled();

    rerender(
      <RowPermissionsProvider
        operators={{}}
        comparators={{}}
        permissions={{ name: { _eq: 'a' } }}
        onPermissionsChange={onChange}
      >
        <Consumer />
      </RowPermissionsProvider>,
    );

    expect(onChange).not.toHaveBeenCalled();
  });

  it('does not re-render in a loop on mount', () => {
    setup({ id: { _eq: 1 } });
    // A couple of renders at most (initial + the single allowed sync pass);
    // a runaway loop would blow well past this.
    expect(renderSpy.mock.calls.length).toBeLessThanOrEqual(2);
  });
});
