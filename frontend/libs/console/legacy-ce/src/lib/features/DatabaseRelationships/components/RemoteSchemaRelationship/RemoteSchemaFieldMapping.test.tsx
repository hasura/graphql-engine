/**
 * React 19 compatibility regression for RemoteSchemaFieldMapping.
 *
 * The component previously set `RemoteSchemaFieldMapping.defaultProps = { ...,
 * onChange: () => {} }`. React 19 IGNORES `defaultProps` on function
 * components, so that default was silently dropped. The default now lives on
 * the destructured parameter (`onChange = () => {}`). These tests assert the
 * real component works under React 19 both when `onChange` is omitted and when
 * it is supplied (including the on-interaction call), proving the default-param
 * fix preserves the prior behaviour.
 *
 * MOCKS (labelled): the heavy `RemoteSchemaTree` sub-tree and `RemoteFieldDisplay`
 * leaf are stubbed — they are not under test. `buildServerRemoteFieldObject`
 * (imported by the component from the same module) is stubbed to a deterministic
 * shape so we can assert exactly what `onChange` receives.
 */
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { buildSchema } from 'graphql';
import { RemoteSchemaFieldMapping } from './RemoteSchemaFieldMapping';

vi.mock('./parts/RemoteSchemaTree', () => ({
  buildServerRemoteFieldObject: (fields: unknown[]) => ({
    fieldCount: fields.length,
  }),
  RemoteSchemaTree: ({
    setRelationshipFields,
  }: {
    setRelationshipFields: (fields: unknown[]) => void;
  }) => (
    <button type="button" onClick={() => setRelationshipFields([{}, {}])}>
      change-mapping
    </button>
  ),
}));

vi.mock('./parts/RemoteFieldDisplay', () => ({
  RemoteFieldDisplay: () => <div data-testid="field-display" />,
}));

const schema = buildSchema('type Query { hello: String }');

describe('RemoteSchemaFieldMapping (React 19 defaultProps -> default params)', () => {
  it('renders without throwing when the optional onChange prop is omitted', () => {
    expect(() =>
      render(<RemoteSchemaFieldMapping graphQLSchema={schema} />),
    ).not.toThrow();
    expect(screen.getByTestId('field-display')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'change-mapping' }),
    ).toBeInTheDocument();
  });

  it('invokes an explicitly supplied onChange on mount and on interaction', async () => {
    const user = userEvent.setup();
    const onChange = vi.fn();
    render(
      <RemoteSchemaFieldMapping graphQLSchema={schema} onChange={onChange} />,
    );

    // Mount effect builds from the initial (empty) mapping -> fieldCount 0.
    expect(onChange).toHaveBeenCalledWith({ fieldCount: 0 });

    const callsBeforeInteraction = onChange.mock.calls.length;
    await user.click(screen.getByRole('button', { name: 'change-mapping' }));

    // The interaction changes the mapping (2 fields) -> onChange fires again.
    expect(onChange.mock.calls.length).toBeGreaterThan(callsBeforeInteraction);
    expect(onChange).toHaveBeenLastCalledWith({ fieldCount: 2 });
  });

  it('does not re-fire onChange when only the onChange identity changes (no runaway renders)', async () => {
    const onChangeA = vi.fn();
    const { rerender } = render(
      <RemoteSchemaFieldMapping graphQLSchema={schema} onChange={onChangeA} />,
    );

    // Mount effect fires exactly once with the initial (empty) mapping.
    await waitFor(() => expect(onChangeA).toHaveBeenCalledTimes(1));

    // Parent re-renders and hands a brand new onChange reference while the
    // relationship fields are unchanged. The effect depends only on the fields
    // (latest onChange is read via a ref), so neither handler should be called.
    const onChangeB = vi.fn();
    rerender(
      <RemoteSchemaFieldMapping graphQLSchema={schema} onChange={onChangeB} />,
    );

    // Give any stray effect a chance to run before asserting stability.
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(onChangeA).toHaveBeenCalledTimes(1);
    expect(onChangeB).not.toHaveBeenCalled();
  });
});
