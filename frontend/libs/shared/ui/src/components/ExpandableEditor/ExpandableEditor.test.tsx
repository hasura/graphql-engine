import '@testing-library/jest-dom/vitest';
import { fireEvent, render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { ExpandableEditor } from './index';

const baseProps = {
  service: 'svc',
  property: 'prop',
  editorExpanded: () => <div>EXPANDED</div>,
  editorCollapsed: () => <div>COLLAPSED</div>,
};

describe('ExpandableEditor', () => {
  it('renders collapsed by default', () => {
    render(
      <Theme>
        <ExpandableEditor {...baseProps} />
      </Theme>,
    );

    expect(screen.getByRole('button', { name: 'Edit' })).toBeInTheDocument();
    expect(screen.getByText('COLLAPSED')).toBeInTheDocument();
    expect(screen.queryByText('EXPANDED')).not.toBeInTheDocument();
  });

  it('follows the controlled `toggled` prop when it changes', () => {
    const { rerender } = render(
      <Theme>
        <ExpandableEditor {...baseProps} toggled={false} />
      </Theme>,
    );

    expect(screen.getByRole('button', { name: 'Edit' })).toBeInTheDocument();

    rerender(
      <Theme>
        <ExpandableEditor {...baseProps} toggled />
      </Theme>,
    );

    expect(screen.getByRole('button', { name: 'Close' })).toBeInTheDocument();
    expect(screen.getByText('EXPANDED')).toBeInTheDocument();
  });

  it('toggles via the button and does not cause a runaway re-render', () => {
    const editorExpanded = vi.fn(() => <div>EXPANDED</div>);

    render(
      <Theme>
        <ExpandableEditor {...baseProps} editorExpanded={editorExpanded} />
      </Theme>,
    );

    // collapsed initially -> expanded renderer not used yet
    expect(editorExpanded).not.toHaveBeenCalled();

    fireEvent.click(screen.getByRole('button', { name: 'Edit' }));

    expect(screen.getByRole('button', { name: 'Close' })).toBeInTheDocument();
    expect(screen.getByText('EXPANDED')).toBeInTheDocument();
    // a single toggle must settle in a bounded number of renders
    expect(editorExpanded.mock.calls.length).toBeLessThanOrEqual(3);
  });

  it('does not re-adjust when an unrelated prop changes', () => {
    const { rerender } = render(
      <Theme>
        <ExpandableEditor {...baseProps} toggled={false} />
      </Theme>,
    );

    // user expands manually
    fireEvent.click(screen.getByRole('button', { name: 'Edit' }));
    expect(screen.getByRole('button', { name: 'Close' })).toBeInTheDocument();

    // re-render with the SAME toggled value: manual expansion must be preserved
    rerender(
      <Theme>
        <ExpandableEditor {...baseProps} toggled={false} dataTest="changed" />
      </Theme>,
    );

    expect(screen.getByRole('button', { name: 'Close' })).toBeInTheDocument();
  });
});
