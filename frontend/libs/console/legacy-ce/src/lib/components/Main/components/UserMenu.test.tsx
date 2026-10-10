import { beforeAll, beforeEach, describe, expect, it, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';

// The @hasura/shared/ui barrel pulls LoadingScreen -> lottie-react -> lottie-web,
// which touches a 2D canvas context that jsdom does not implement. Stub the
// animation component; it is irrelevant to the user menu.
vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

import { AppTheme } from '@hasura/shared/ui';
import UserMenu from './UserMenu';

const root = () => document.documentElement;
// Radix opens the menu on pointerdown / keydown (not a bare click). Enter also
// exercises the trigger's keyboard operability.
const openMenu = () => {
  const trigger = screen.getByRole('button', { name: /user menu/i });
  trigger.focus();
  fireEvent.keyDown(trigger, { key: 'Enter', code: 'Enter' });
};

beforeAll(() => {
  // jsdom gaps that Radix's menu / scroll-area rely on.
  if (!('ResizeObserver' in globalThis)) {
    (globalThis as unknown as { ResizeObserver: unknown }).ResizeObserver =
      class {
        observe() {}
        unobserve() {}
        disconnect() {}
      };
  }
  if (!Element.prototype.hasPointerCapture) {
    Element.prototype.hasPointerCapture = () => false;
  }
  if (!Element.prototype.scrollIntoView) {
    Element.prototype.scrollIntoView = () => undefined;
  }
});

beforeEach(() => {
  window.localStorage.clear();
  root().classList.remove('light', 'dark');
});

describe('UserMenu', () => {
  it('exposes a keyboard-accessible <button> trigger with an accessible name', () => {
    render(
      <AppTheme>
        <UserMenu />
      </AppTheme>,
    );
    const trigger = screen.getByRole('button', { name: /user menu/i });
    expect(trigger.tagName).toBe('BUTTON');
  });

  it('shows the Dark mode toggle alongside the existing social links', () => {
    render(
      <AppTheme>
        <UserMenu />
      </AppTheme>,
    );
    openMenu();
    expect(
      screen.getByRole('menuitem', { name: /dark mode/i }),
    ).toBeInTheDocument();
    expect(screen.getByText('Star')).toBeInTheDocument();
    expect(screen.getByText('Tweet')).toBeInTheDocument();
  });

  it('retains custom items passed via props', () => {
    render(
      <AppTheme>
        <UserMenu items={[<div key="custom">Custom Item</div>]} />
      </AppTheme>,
    );
    openMenu();
    expect(screen.getByText('Custom Item')).toBeInTheDocument();
  });

  it('toggles dark mode from the menu', () => {
    render(
      <AppTheme>
        <UserMenu />
      </AppTheme>,
    );
    openMenu();
    fireEvent.click(screen.getByRole('menuitem', { name: /dark mode/i }));
    expect(root()).toHaveClass('dark');
  });
});
