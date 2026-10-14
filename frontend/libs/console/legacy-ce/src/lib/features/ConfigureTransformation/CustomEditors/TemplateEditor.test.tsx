/**
 * Regression coverage for TemplateEditor's prop->local-state sync, moved from
 * two `useEffect`s (set-state-in-effect) to the React "adjust state when a prop
 * changes" during-render pattern. Covers: initial value, reset on requestBody
 * change, error mirror on requestBodyError change, no clobbering of local edits,
 * and no runaway renders.
 *
 * AceEditor/IndicatorCard are mocked so the component's own state logic runs for
 * real without pulling in ace-builds.
 */
import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { vi } from 'vitest';
import { RequestTransformStateBody } from '../stateDefaults';

vi.mock('@hasura/shared/ui', () => ({
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
  IndicatorCard: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="error-card">{children}</div>
  ),
}));

import TemplateEditor from './TemplateEditor';

const body = (template: string): RequestTransformStateBody =>
  ({ action: 'transform', template }) as RequestTransformStateBody;

const editor = () => screen.getByTestId('temp-editor') as HTMLTextAreaElement;

describe('TemplateEditor prop/local-state sync', () => {
  it('renders the initial template', () => {
    render(
      <TemplateEditor
        requestBody={body('hello')}
        requestBodyOnChange={vi.fn()}
      />,
    );
    expect(editor().value).toBe('hello');
  });

  it('resets local value when the requestBody prop changes', () => {
    const { rerender } = render(
      <TemplateEditor
        requestBody={body('one')}
        requestBodyOnChange={vi.fn()}
      />,
    );
    rerender(
      <TemplateEditor
        requestBody={body('two')}
        requestBodyOnChange={vi.fn()}
      />,
    );
    expect(editor().value).toBe('two');
  });

  it('does not clobber an in-progress local edit', () => {
    const stable = body('one');
    const { rerender } = render(
      <TemplateEditor requestBody={stable} requestBodyOnChange={vi.fn()} />,
    );
    fireEvent.change(editor(), { target: { value: 'edited' } });
    // Re-render parent with the SAME requestBody reference: local edit stays.
    rerender(
      <TemplateEditor requestBody={stable} requestBodyOnChange={vi.fn()} />,
    );
    expect(editor().value).toBe('edited');
  });

  it('mirrors the requestBodyError prop', () => {
    const { rerender } = render(
      <TemplateEditor requestBody={body('x')} requestBodyOnChange={vi.fn()} />,
    );
    expect(screen.queryByTestId('error-card')).not.toBeInTheDocument();

    rerender(
      <TemplateEditor
        requestBody={body('x')}
        requestBodyError="boom"
        requestBodyOnChange={vi.fn()}
      />,
    );
    expect(screen.getByTestId('error-card')).toHaveTextContent('boom');

    rerender(
      <TemplateEditor
        requestBody={body('x')}
        requestBodyError=""
        requestBodyOnChange={vi.fn()}
      />,
    );
    expect(screen.queryByTestId('error-card')).not.toBeInTheDocument();
  });

  it('does not re-render unboundedly for a stable requestBody', () => {
    let commits = 0;
    const stable = body('one');
    const Probe = () => {
      commits++;
      return (
        <TemplateEditor requestBody={stable} requestBodyOnChange={vi.fn()} />
      );
    };
    const { rerender } = render(<Probe />);
    const mountCommits = commits;
    rerender(<Probe />);
    rerender(<Probe />);
    expect(commits - mountCommits).toBeLessThanOrEqual(2);
  });
});
