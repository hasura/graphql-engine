import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { NameValue } from '@hasura/shared/types';
import SampleContextTransforms from './SampleContextTransforms';

const renderCmp = (
  envVars: NameValue[],
  sessionVars: NameValue[] = [{ name: '', value: '' }],
) => {
  const props = {
    transformationType: 'action' as const,
    envVars,
    sessionVars,
    envVarsOnChange: vi.fn(),
    sessionVarsOnChange: vi.fn(),
  };
  const node = (p: typeof props) => (
    <Theme>
      <SampleContextTransforms {...p} />
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

describe('SampleContextTransforms prop/local-state sync', () => {
  it('renders the initial env vars', () => {
    renderCmp([{ name: 'MY_ENV', value: 'v' }]);
    expect(keyInputs().some((i) => i.value === 'MY_ENV')).toBe(true);
  });

  it('resets local env vars when the envVars prop changes', () => {
    const { rerenderWith } = renderCmp([{ name: 'OLD', value: 'v' }]);
    expect(keyInputs().some((i) => i.value === 'OLD')).toBe(true);
    rerenderWith({ envVars: [{ name: 'NEW', value: 'v2' }] });
    expect(keyInputs().some((i) => i.value === 'NEW')).toBe(true);
    expect(keyInputs().some((i) => i.value === 'OLD')).toBe(false);
  });

  it('does not clobber a local edit when an unrelated prop re-renders', () => {
    const stableEnv: NameValue[] = [{ name: 'KEEP', value: 'v' }];
    const { rerenderWith } = renderCmp(stableEnv);
    const target = keyInputs().find(
      (i) => i.value === 'KEEP',
    ) as HTMLInputElement;
    fireEvent.change(target, { target: { value: 'EDITED' } });
    expect(keyInputs().some((i) => i.value === 'EDITED')).toBe(true);
    // Re-render with the SAME envVars reference but a changed sessionVars.
    rerenderWith({ sessionVars: [{ name: 'S', value: 's' }] });
    expect(keyInputs().some((i) => i.value === 'EDITED')).toBe(true);
  });

  it('does not re-render unboundedly for stable props', () => {
    let commits = 0;
    const env: NameValue[] = [{ name: 'E', value: 'v' }];
    const session: NameValue[] = [{ name: 'S', value: 'v' }];
    const Probe = () => {
      commits++;
      return (
        <Theme>
          <SampleContextTransforms
            transformationType="action"
            envVars={env}
            sessionVars={session}
            envVarsOnChange={vi.fn()}
            sessionVarsOnChange={vi.fn()}
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
