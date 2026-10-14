/**
 * Regression coverage for PayloadOptionsTransforms, whose requestBody->local
 * form sync (set-state-in-effect) was replaced with the React "adjust state when
 * a prop changes" during-render pattern. Covers: initial form elements, resync
 * on requestBody change, no clobbering of local edits, and no runaway renders.
 *
 * AceEditor is mocked (ace-builds); the real KeyValueInput runs.
 */
import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { NameValue } from '@hasura/shared/types';
import { RequestTransformStateBody } from './stateDefaults';
import { requestBodyActionState } from './requestTransformState';

// Radix Select (rendered by this component) relies on ResizeObserver, which
// jsdom does not implement.
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

import PayloadOptionsTransforms from './PayloadOptionsTransforms';

const body = (form_template: NameValue[]): RequestTransformStateBody =>
  ({
    action: requestBodyActionState.transformFormUrlEncoded,
    form_template,
  }) as RequestTransformStateBody;

const renderCmp = (requestBody: RequestTransformStateBody) => {
  const props = {
    transformationType: 'action' as const,
    requestBody,
    requestBodyError: '',
    requestSampleInput: '',
    requestTransformedBody: '',
    resetSampleInput: vi.fn(),
    requestBodyOnChange: vi.fn(),
    requestSampleInputOnChange: vi.fn(),
  };
  const node = (p: typeof props) => (
    <Theme>
      <PayloadOptionsTransforms {...p} />
    </Theme>
  );
  const utils = render(node(props));
  return {
    ...utils,
    rerenderWith: (patch: Partial<typeof props>) =>
      utils.rerender(node({ ...props, ...patch })),
  };
};

const keyInputs = () =>
  screen.getAllByPlaceholderText('Key...') as HTMLInputElement[];

describe('PayloadOptionsTransforms prop/local-state sync', () => {
  it('renders the initial form elements', () => {
    renderCmp(body([{ name: 'FOO', value: '1' }]));
    expect(keyInputs().some((i) => i.value === 'FOO')).toBe(true);
  });

  it('resets local form elements when requestBody changes', () => {
    const { rerenderWith } = renderCmp(body([{ name: 'OLD', value: '1' }]));
    rerenderWith({ requestBody: body([{ name: 'NEW', value: '2' }]) });
    expect(keyInputs().some((i) => i.value === 'NEW')).toBe(true);
    expect(keyInputs().some((i) => i.value === 'OLD')).toBe(false);
  });

  it('does not clobber a local edit when an unrelated prop re-renders', () => {
    const stable = body([{ name: 'KEEP', value: '1' }]);
    const { rerenderWith } = renderCmp(stable);
    const target = keyInputs().find(
      (i) => i.value === 'KEEP',
    ) as HTMLInputElement;
    fireEvent.change(target, { target: { value: 'EDITED' } });
    expect(keyInputs().some((i) => i.value === 'EDITED')).toBe(true);
    rerenderWith({ requestTransformedBody: 'changed' });
    expect(keyInputs().some((i) => i.value === 'EDITED')).toBe(true);
  });

  it('does not re-render unboundedly for a stable requestBody', () => {
    let commits = 0;
    const stable = body([{ name: 'E', value: '1' }]);
    const Probe = () => {
      commits++;
      return (
        <Theme>
          <PayloadOptionsTransforms
            transformationType="action"
            requestBody={stable}
            requestBodyError=""
            requestSampleInput=""
            requestTransformedBody=""
            resetSampleInput={vi.fn()}
            requestBodyOnChange={vi.fn()}
            requestSampleInputOnChange={vi.fn()}
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
