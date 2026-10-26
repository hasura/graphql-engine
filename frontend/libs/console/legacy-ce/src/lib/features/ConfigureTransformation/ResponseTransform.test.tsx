import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { ResponseTransformStateBody } from './stateDefaults';
import { responseBodyActionState } from './requestTransformState';

vi.mock('@hasura/shared/ui', async (importActual) => {
  const actual = await importActual<typeof import('@hasura/shared/ui')>();
  return {
    ...actual,
    AceEditor: ({
      value,
      onChange,
      name,
    }: {
      value: string;
      onChange: (v: string) => void;
      name: string;
    }) => (
      <textarea
        data-testid={name}
        value={value}
        onChange={(e) => onChange(e.target.value)}
      />
    ),
  };
});

import ResponseTransforms from './ResponseTransform';

const body = (template: string): ResponseTransformStateBody =>
  ({
    action: responseBodyActionState.transformApplicationJson,
    template,
  }) as ResponseTransformStateBody;

const editor = () => screen.getByTestId('temp-editor') as HTMLTextAreaElement;

describe('ResponseTransforms prop/local-state sync', () => {
  it('renders the nested editor for the applicationJson action', () => {
    render(
      <Theme>
        <ResponseTransforms
          responseBody={body('tmpl')}
          responseBodyOnChange={vi.fn()}
        />
      </Theme>,
    );
    expect(editor().value).toBe('tmpl');
  });

  it('reflects a responseBody change', () => {
    const node = (b: ResponseTransformStateBody) => (
      <Theme>
        <ResponseTransforms responseBody={b} responseBodyOnChange={vi.fn()} />
      </Theme>
    );
    const { rerender } = render(node(body('one')));
    rerender(node(body('two')));
    expect(editor().value).toBe('two');
  });

  it('does not re-render unboundedly for a stable responseBody', () => {
    let commits = 0;
    const stable = body('x');
    const Probe = () => {
      commits++;
      return (
        <Theme>
          <ResponseTransforms
            responseBody={stable}
            responseBodyOnChange={vi.fn()}
          />
        </Theme>
      );
    };
    const { rerender } = render(<Probe />);
    const mountCommits = commits;
    rerender(<Probe />);
    rerender(<Probe />);
    expect(commits - mountCommits).toBeLessThanOrEqual(2);
  });
});
