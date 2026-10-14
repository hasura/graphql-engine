import { render, screen, waitFor } from '@testing-library/react';
import { ArgSelect } from './ArgSelect';

// MOCKS (labelled): the leaf input and the child-argument resolver are stubbed
// so we exercise the leaf render path and can read the `editMode` flag the
// component derives from its `value` prop.
let renderCount = 0;

vi.mock('./RSPInput', () => ({
  default: ({ editMode }: { editMode: boolean }) => {
    renderCount += 1;
    return <div data-testid="rsp">editMode:{String(editMode)}</div>;
  },
}));

vi.mock('./utils', () => ({
  getChildArguments: () => ({ children: undefined }),
}));

const renderArgSelect = (value: unknown) =>
  render(
    <ArgSelect
      keyName="arg"
      valueField={{} as never}
      value={value as never}
      level={0}
      setArg={() => undefined}
    />,
  );

beforeEach(() => {
  renderCount = 0;
});

describe('ArgSelect (edit-mode sync during render)', () => {
  it('starts in edit mode when a non-empty string value is already present', () => {
    renderArgSelect('hello');
    expect(screen.getByTestId('rsp')).toHaveTextContent('editMode:true');
  });

  it('starts with the pen (editMode false) when there is no value', () => {
    renderArgSelect(undefined);
    expect(screen.getByTestId('rsp')).toHaveTextContent('editMode:false');
  });

  it('switches to edit mode when a value arrives later, without looping', async () => {
    const { rerender } = renderArgSelect(undefined);
    expect(screen.getByTestId('rsp')).toHaveTextContent('editMode:false');

    rerender(
      <ArgSelect
        keyName="arg"
        valueField={{} as never}
        value={'hello' as never}
        level={0}
        setArg={() => undefined}
      />,
    );

    expect(screen.getByTestId('rsp')).toHaveTextContent('editMode:true');

    // The render-phase setState converges in a single extra render (guarded by
    // !editMode); allow a tick and assert the render count stays bounded.
    await new Promise((resolve) => setTimeout(resolve, 50));
    await waitFor(() =>
      expect(screen.getByTestId('rsp')).toHaveTextContent('editMode:true'),
    );
    expect(renderCount).toBeLessThan(6);
  });
});
