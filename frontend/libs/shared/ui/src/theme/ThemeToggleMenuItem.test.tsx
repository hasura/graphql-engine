import { beforeEach, describe, expect, it } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { DropdownMenu } from '@radix-ui/themes';
import { AppTheme } from './AppTheme';
import { ThemeToggleMenuItem } from './ThemeToggleMenuItem';
import { APPEARANCE_STORAGE_KEY } from './appearance';

const root = () => document.documentElement;

const renderMenu = () =>
  render(
    <AppTheme>
      <DropdownMenu.Root defaultOpen>
        <DropdownMenu.Trigger>
          <button type="button">open</button>
        </DropdownMenu.Trigger>
        <DropdownMenu.Content>
          <ThemeToggleMenuItem />
        </DropdownMenu.Content>
      </DropdownMenu.Root>
    </AppTheme>,
  );

beforeEach(() => {
  window.localStorage.clear();
  root().classList.remove('light', 'dark');
  root().style.colorScheme = '';
});

// NOTE: the toggle is an ordinary action `menuitem` with a contextual label
// ("Dark mode" in light, "Light mode" in dark) — selecting it toggles appearance
// and closes the menu, like any other action item. The current appearance is
// asserted via the document root class + persisted localStorage value (the source
// of truth), not via `aria-checked`. Because selecting closes the menu, the
// "already dark" direction is exercised by seeding the stored appearance and
// rendering a fresh menu rather than reopening (which keeps the test off Radix's
// portal/focus re-open timing in jsdom).
describe('ThemeToggleMenuItem', () => {
  it('renders an accessible "Dark mode" action item while in light appearance', () => {
    renderMenu();
    const item = screen.getByRole('menuitem', { name: /dark mode/i });
    expect(item).toBeInTheDocument();
    // a plain action item, not a checkbox item
    expect(item).not.toHaveAttribute('aria-checked');
    expect(root().classList.contains('dark')).toBe(false);
  });

  it('toggles to dark on select and persists to the document root', () => {
    renderMenu();
    fireEvent.click(screen.getByRole('menuitem', { name: /dark mode/i }));
    expect(root().classList.contains('dark')).toBe(true);
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('dark');
  });

  it('shows the contextual "Light mode" label and toggles back to light when already dark', () => {
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'dark');
    renderMenu();
    // the contextual label reflects the current (dark) appearance
    expect(
      screen.queryByRole('menuitem', { name: /dark mode/i }),
    ).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('menuitem', { name: /light mode/i }));
    expect(root().classList.contains('dark')).toBe(false);
    expect(root().classList.contains('light')).toBe(true);
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('light');
  });

  it('toggles via keyboard Enter (light -> dark)', () => {
    renderMenu();
    const darkItem = screen.getByRole('menuitem', { name: /dark mode/i });
    darkItem.focus();
    fireEvent.keyDown(darkItem, { key: 'Enter', code: 'Enter' });
    expect(root().classList.contains('dark')).toBe(true);
  });

  it('toggles via keyboard Space (dark -> light)', () => {
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'dark');
    renderMenu();
    const lightItem = screen.getByRole('menuitem', { name: /light mode/i });
    lightItem.focus();
    fireEvent.keyDown(lightItem, { key: ' ', code: 'Space' });
    expect(root().classList.contains('dark')).toBe(false);
  });
});
