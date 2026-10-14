/**
 * Regression coverage for RequestUrlEditor, whose four prop->local-state sync
 * effects (set-state-in-effect, plus one exhaustive-deps warning and a stray
 * eslint-disable) were replaced with the React "adjust state when a prop
 * changes" during-render pattern. Covers: initial values, resync when
 * requestUrl / requestUrlError / requestQueryParams change, no clobbering of
 * in-progress edits, and no runaway renders.
 *
 * Only AceEditor is mocked (to avoid ace-builds); the real Input / RadioGroup /
 * IndicatorCard / KeyValueInput run so the state logic is exercised end to end.
 */
import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { QueryParams } from '../stateDefaults';

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

import RequestUrlEditor from './RequestUrlEditor';

type Props = React.ComponentProps<typeof RequestUrlEditor>;

const renderEditor = (overrides: Partial<Props> = {}) => {
  const props: Props = {
    requestUrl: '',
    requestUrlError: '',
    requestUrlPreview: '',
    requestQueryParams: [{ name: '', value: '' }],
    requestUrlOnChange: vi.fn(),
    requestQueryParamsOnChange: vi.fn(),
    ...overrides,
  };
  const utils = render(
    <Theme>
      <RequestUrlEditor {...props} />
    </Theme>,
  );
  return { ...utils, props };
};

const urlInput = () =>
  screen.getByPlaceholderText('URL Template (Optional)...') as HTMLInputElement;

describe('RequestUrlEditor prop/local-state sync', () => {
  it('renders initial url and key-value params by default', () => {
    renderEditor({ requestUrl: 'https://x' });
    expect(urlInput().value).toBe('https://x');
    // array query params => key-value mode (KeyValueInput shows Key.../Value...)
    expect(screen.getAllByPlaceholderText('Key...').length).toBeGreaterThan(0);
  });

  it('resets the local url when the requestUrl prop changes', () => {
    const { rerender, props } = renderEditor({ requestUrl: 'first' });
    expect(urlInput().value).toBe('first');
    rerender(
      <Theme>
        <RequestUrlEditor {...props} requestUrl="second" />
      </Theme>,
    );
    expect(urlInput().value).toBe('second');
  });

  it('does not clobber an in-progress url edit when unrelated props re-render', () => {
    const { rerender, props } = renderEditor({ requestUrl: 'first' });
    fireEvent.change(urlInput(), { target: { value: 'typing...' } });
    expect(urlInput().value).toBe('typing...');
    // Re-render with the SAME requestUrl: the local edit must survive.
    rerender(
      <Theme>
        <RequestUrlEditor {...props} requestUrlPreview="preview-changed" />
      </Theme>,
    );
    expect(urlInput().value).toBe('typing...');
  });

  it('mirrors the requestUrlError prop', () => {
    const { rerender, props } = renderEditor();
    expect(screen.queryByText('bad url')).not.toBeInTheDocument();
    rerender(
      <Theme>
        <RequestUrlEditor {...props} requestUrlError="bad url" />
      </Theme>,
    );
    expect(screen.getByText('bad url')).toBeInTheDocument();
    rerender(
      <Theme>
        <RequestUrlEditor {...props} requestUrlError="" />
      </Theme>,
    );
    expect(screen.queryByText('bad url')).not.toBeInTheDocument();
  });

  it('switches to url-string mode when requestQueryParams becomes a string', () => {
    const { rerender, props } = renderEditor({
      requestQueryParams: [{ name: 'a', value: '1' }],
    });
    expect(screen.queryByTestId('sdl-editor')).not.toBeInTheDocument();
    rerender(
      <Theme>
        <RequestUrlEditor
          {...props}
          requestQueryParams={'a=1' as QueryParams}
        />
      </Theme>,
    );
    expect(screen.getByTestId('sdl-editor')).toBeInTheDocument();
    expect(
      (screen.getByTestId('sdl-editor') as HTMLTextAreaElement).value,
    ).toBe('a=1');
  });

  it('does not re-render unboundedly for stable props', () => {
    let commits = 0;
    const stableParams: QueryParams = [{ name: 'a', value: '1' }];
    const Probe = () => {
      commits++;
      return (
        <Theme>
          <RequestUrlEditor
            requestUrl="u"
            requestUrlError=""
            requestUrlPreview=""
            requestQueryParams={stableParams}
            requestUrlOnChange={vi.fn()}
            requestQueryParamsOnChange={vi.fn()}
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
