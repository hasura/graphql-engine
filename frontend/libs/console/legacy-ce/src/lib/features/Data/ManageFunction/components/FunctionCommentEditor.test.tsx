import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { FunctionCommentEditor } from './FunctionCommentEditor';

const mutate = vi.fn((args: { onSuccess?: () => void }) => {
  args.onSuccess?.();
});

vi.mock('../../hooks/useSetFunctionConfiguration', () => ({
  useSetFunctionConfiguration: () => ({
    setFunctionConfiguration: mutate,
    isPending: false,
  }),
}));

vi.mock('../../components/LoadDatabaseCommentButton', () => ({
  LoadDatabaseCommentButton: ({
    onLoad,
  }: {
    onLoad: (comment: string) => void;
  }) => (
    <button type="button" onClick={() => onLoad('comment from db')}>
      Load from database
    </button>
  ),
}));

const source = { name: 'default', kind: 'postgres' } as any;
const func = {
  function: { name: 'my_func', schema: 'public' },
  configuration: { custom_name: 'myFunc', comment: 'existing comment' },
} as any;

const setup = (
  props?: Partial<Parameters<typeof FunctionCommentEditor>[0]>,
) => {
  const onSuccess = vi.fn();
  render(
    <FunctionCommentEditor
      defaultValue="existing comment"
      readOnly={false}
      source={source}
      func={func}
      onSuccess={onSuccess}
      {...props}
    />,
  );
  return { onSuccess };
};

describe('FunctionCommentEditor', () => {
  beforeEach(() => {
    mutate.mockClear();
  });

  it('shows the current comment and an edit trigger, with no dialog initially', () => {
    setup();
    expect(screen.getByText('existing comment')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /edit comment/i }),
    ).toBeInTheDocument();
    expect(screen.queryByLabelText('Function comment')).not.toBeInTheDocument();
  });

  it('opens a dialog seeded with the current comment when the trigger is clicked', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    expect(input).toHaveValue('existing comment');
  });

  it('saves the edited comment into the function configuration and closes on success', async () => {
    const { onSuccess } = setup();
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    await userEvent.clear(input);
    await userEvent.type(input, 'updated comment');
    await userEvent.click(screen.getByRole('button', { name: /save/i }));

    // other configuration fields are kept
    expect(mutate).toHaveBeenCalledWith(
      expect.objectContaining({
        qualifiedFunction: func.function,
        configuration: { custom_name: 'myFunc', comment: 'updated comment' },
      }),
    );
    expect(onSuccess).toHaveBeenCalledTimes(1);
    await waitFor(() =>
      expect(
        screen.queryByLabelText('Function comment'),
      ).not.toBeInTheDocument(),
    );
  });

  it('supports clearing the comment (comment is removed from the configuration)', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    await userEvent.clear(input);
    await userEvent.click(screen.getByRole('button', { name: /save/i }));

    expect(mutate).toHaveBeenCalledWith(
      expect.objectContaining({ configuration: { custom_name: 'myFunc' } }),
    );
  });

  it('cancel closes the dialog without persisting', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    await userEvent.clear(input);
    await userEvent.type(input, 'discard me');
    // The dialog footer renders "Cancel" as a Radix Dialog.Close (not a button role).
    await userEvent.click(screen.getByText('Cancel'));

    expect(mutate).not.toHaveBeenCalled();
    await waitFor(() =>
      expect(
        screen.queryByLabelText('Function comment'),
      ).not.toBeInTheDocument(),
    );
  });

  it('fills the input from the database comment without saving', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    await userEvent.click(
      screen.getByRole('button', { name: /load from database/i }),
    );
    expect(await screen.findByLabelText('Function comment')).toHaveValue(
      'comment from db',
    );
    expect(mutate).not.toHaveBeenCalled();
  });

  it('shows "Add a Comment" and no card when there is no comment', () => {
    setup({ defaultValue: '' });
    expect(
      screen.getByRole('button', { name: /add a comment/i }),
    ).toBeInTheDocument();
  });

  it('hides the edit trigger in read-only mode', () => {
    setup({ readOnly: true });
    expect(
      screen.queryByRole('button', { name: /edit comment/i }),
    ).not.toBeInTheDocument();
  });

  it('discards the draft and closes when the function changes while editing (even if the comment is unchanged)', async () => {
    const funcA = { function: { name: 'func_a', schema: 'public' } } as any;
    const funcB = { function: { name: 'func_b', schema: 'public' } } as any;
    const onSuccess = vi.fn();

    const { rerender } = render(
      <FunctionCommentEditor
        defaultValue="shared comment"
        readOnly={false}
        source={source}
        func={funcA}
        onSuccess={onSuccess}
      />,
    );

    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    await userEvent.clear(input);
    await userEvent.type(input, 'draft for func_a');

    // Switch to a different function with an IDENTICAL defaultValue.
    rerender(
      <FunctionCommentEditor
        defaultValue="shared comment"
        readOnly={false}
        source={source}
        func={funcB}
        onSuccess={onSuccess}
      />,
    );

    // The dialog closes and the stale draft is discarded.
    await waitFor(() =>
      expect(
        screen.queryByLabelText('Function comment'),
      ).not.toBeInTheDocument(),
    );

    // Reopening shows the target's own comment, not the stale draft, and a
    // save submits against the NEW function (never "draft for func_a").
    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const reopened = await screen.findByLabelText('Function comment');
    expect(reopened).toHaveValue('shared comment');
    await userEvent.click(screen.getByRole('button', { name: /save/i }));

    expect(mutate).toHaveBeenCalledTimes(1);
    expect(mutate).toHaveBeenCalledWith(
      expect.objectContaining({
        qualifiedFunction: funcB.function,
        configuration: { comment: 'shared comment' },
      }),
    );
  });

  it('discards the draft and closes when the source changes while editing', async () => {
    const sourceA = { name: 'source_a', kind: 'postgres' } as any;
    const sourceB = { name: 'source_b', kind: 'postgres' } as any;
    const onSuccess = vi.fn();

    const { rerender } = render(
      <FunctionCommentEditor
        defaultValue="shared comment"
        readOnly={false}
        source={sourceA}
        func={func}
        onSuccess={onSuccess}
      />,
    );

    await userEvent.click(
      screen.getByRole('button', { name: /edit comment/i }),
    );
    const input = await screen.findByLabelText('Function comment');
    await userEvent.clear(input);
    await userEvent.type(input, 'draft for source_a');

    rerender(
      <FunctionCommentEditor
        defaultValue="shared comment"
        readOnly={false}
        source={sourceB}
        func={func}
        onSuccess={onSuccess}
      />,
    );

    await waitFor(() =>
      expect(
        screen.queryByLabelText('Function comment'),
      ).not.toBeInTheDocument(),
    );
    expect(mutate).not.toHaveBeenCalled();
  });
});
