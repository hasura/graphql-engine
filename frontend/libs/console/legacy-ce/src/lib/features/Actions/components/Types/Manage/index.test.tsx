import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import TypesManage from './index';

// Control the metadata returned to the component across renders.
let currentAllTypes: unknown = { type_a: 1 };

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({ data: currentAllTypes }),
}));

vi.mock('@hasura/shared/context', async (importActual) => {
  const actual = await importActual<Record<string, unknown>>();
  return { ...actual, useAppContext: () => ({ readOnlyMode: false }) };
});

vi.mock('../../../hooks/useSetCustomGraphQLTypes', () => ({
  default: () => () => Promise.resolve(),
}));

const { buildTypeSDLSpy } = vi.hoisted(() => ({
  buildTypeSDLSpy: vi.fn((types: unknown) => `SDL:${JSON.stringify(types)}`),
}));

vi.mock('../../../../../shared/utils/sdlUtils', () => ({
  buildTypeSDL: buildTypeSDLSpy,
  buildCustomTypesFromSDL: () => ({ types: [], error: null }),
}));

vi.mock('../CustomTypesContainer', () => ({
  default: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
}));

vi.mock('../../Common/GraphQLEditor', () => ({
  default: ({ value }: { value?: string }) => (
    <textarea data-testid="sdl-editor" readOnly value={value} />
  ),
}));

describe('TypesManage', () => {
  beforeEach(() => {
    currentAllTypes = { type_a: 1 };
  });

  it('seeds the editor from metadata on initial render', () => {
    render(
      <Theme>
        <TypesManage />
      </Theme>,
    );
    expect(screen.getByTestId('sdl-editor')).toHaveValue(
      `SDL:${JSON.stringify({ type_a: 1 })}`,
    );
  });

  it('re-seeds the editor when the metadata changes', () => {
    const { rerender } = render(
      <Theme>
        <TypesManage />
      </Theme>,
    );
    expect(screen.getByTestId('sdl-editor')).toHaveValue(
      `SDL:${JSON.stringify({ type_a: 1 })}`,
    );

    // new metadata reference => editor resets to the new server SDL
    currentAllTypes = { type_b: 2 };
    rerender(
      <Theme>
        <TypesManage />
      </Theme>,
    );
    expect(screen.getByTestId('sdl-editor')).toHaveValue(
      `SDL:${JSON.stringify({ type_b: 2 })}`,
    );
  });

  it('does not loop when metadata is unchanged across re-renders', () => {
    const { rerender } = render(
      <Theme>
        <TypesManage />
      </Theme>,
    );
    const callsAfterMount = buildTypeSDLSpy.mock.calls.length;

    rerender(
      <Theme>
        <TypesManage />
      </Theme>,
    );
    // same metadata reference => no extra re-seed during the stable re-render
    expect(buildTypeSDLSpy.mock.calls.length).toBe(callsAfterMount);
  });
});
