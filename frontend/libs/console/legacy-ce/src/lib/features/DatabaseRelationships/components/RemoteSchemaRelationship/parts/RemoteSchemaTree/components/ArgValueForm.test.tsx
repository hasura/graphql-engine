import type { Dispatch, SetStateAction } from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { GraphQLString } from 'graphql';
import { AppTheme } from '@hasura/shared/ui';
import { ArgValueForm } from './ArgValueForm';
import type { ArgValue, RelationshipFields } from '../../../types';

// jsdom does not implement ResizeObserver, which the Radix-based `Select`
// (rendered by ArgValueForm's "Fill From" picker) relies on.
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

const buildField = (argValue: ArgValue): RelationshipFields => ({
  key: 'arg-key',
  argValue,
  checkable: true,
  depth: 1,
  type: 'arg',
});

type SetupOptions = {
  setRelationshipFields?: Dispatch<SetStateAction<RelationshipFields[]>>;
};

const setup = (
  argValue: ArgValue,
  { setRelationshipFields }: SetupOptions = {},
) => {
  const Wrapper = ({ arg }: { arg: ArgValue }) => (
    <AppTheme>
      <ArgValueForm
        argKey="arg-key"
        relationshipFields={[buildField(arg)]}
        setRelationshipFields={setRelationshipFields ?? (() => undefined)}
        argValue={arg}
        fields={['id', 'name']}
        argType={GraphQLString}
      />
    </AppTheme>
  );

  const view = render(<Wrapper arg={argValue} />);
  return {
    ...view,
    rerenderWith: (arg: ArgValue) => view.rerender(<Wrapper arg={arg} />),
    // For a String scalar the static value is exposed via a single text input.
    input: () => screen.getByRole('textbox') as HTMLInputElement,
  };
};

const staticArg = (value: string): ArgValue => ({
  kind: 'static',
  value,
  type: 'String',
});

describe('ArgValueForm (argValue prop sync)', () => {
  it('renders the initial argValue', () => {
    setup(staticArg('hello'));
    expect(screen.getByDisplayValue('hello')).toBeInTheDocument();
  });

  it('resets the local value when the argValue prop changes (render-phase sync, not an effect)', () => {
    const { rerenderWith } = setup(staticArg('hello'));
    expect(screen.getByDisplayValue('hello')).toBeInTheDocument();

    rerenderWith(staticArg('world'));

    expect(screen.getByDisplayValue('world')).toBeInTheDocument();
    expect(screen.queryByDisplayValue('hello')).not.toBeInTheDocument();
  });

  it('preserves a local edit across a re-render with the same argValue reference', () => {
    const sameArg = staticArg('hello');
    const { rerenderWith, input } = setup(sameArg);

    fireEvent.change(input(), { target: { value: 'locally-edited' } });
    expect(screen.getByDisplayValue('locally-edited')).toBeInTheDocument();

    // Re-render with the identical prop reference: the render-phase guard must
    // NOT clobber the user's in-progress edit.
    rerenderWith(sameArg);
    expect(screen.getByDisplayValue('locally-edited')).toBeInTheDocument();
  });

  it('pushes exactly one debounced update after a prop reset and does not re-render in a loop', async () => {
    const setRelationshipFields = vi.fn();
    const { rerenderWith } = setup(staticArg('hello'), {
      setRelationshipFields,
    });

    // Reset synchronously (before the mount debounce can fire) so only the reset
    // value is propagated.
    rerenderWith(staticArg('world'));

    await waitFor(() => expect(setRelationshipFields).toHaveBeenCalledTimes(1));
    const pushed = setRelationshipFields.mock
      .calls[0][0] as RelationshipFields[];
    expect(pushed[0].argValue?.value).toBe('world');

    // No runaway: once settled, nothing schedules further updates.
    await new Promise((resolve) => setTimeout(resolve, 500));
    expect(setRelationshipFields).toHaveBeenCalledTimes(1);
  });
});
