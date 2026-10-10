import { beforeEach, describe, expect, it, vi } from 'vitest';
import { StrictMode } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { ThemeProvider, useAppearance } from './ThemeProvider';
import { APPEARANCE_STORAGE_KEY } from './appearance';

function Probe() {
  const { appearance, toggleAppearance, setAppearance } = useAppearance();
  return (
    <div>
      <span data-testid="appearance">{appearance}</span>
      <button onClick={toggleAppearance}>toggle</button>
      <button onClick={() => setAppearance('dark')}>set-dark</button>
      <button onClick={() => setAppearance('light')}>set-light</button>
    </div>
  );
}

const root = () => document.documentElement;

beforeEach(() => {
  window.localStorage.clear();
  root().classList.remove('light', 'dark');
  root().style.colorScheme = '';
  vi.restoreAllMocks();
});

describe('ThemeProvider', () => {
  it('defaults to light and marks the document root', () => {
    render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    expect(screen.getByTestId('appearance').textContent).toBe('light');
    expect(root().classList.contains('light')).toBe(true);
    expect(root().classList.contains('dark')).toBe(false);
    expect(root().style.colorScheme).toBe('light');
  });

  it('initializes from a stored dark preference', () => {
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'dark');
    render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    expect(screen.getByTestId('appearance').textContent).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
    expect(root().classList.contains('light')).toBe(false);
    expect(root().style.colorScheme).toBe('dark');
  });

  it('toggles, updates the document root, and persists', () => {
    render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    fireEvent.click(screen.getByText('toggle'));
    expect(screen.getByTestId('appearance').textContent).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
    expect(root().classList.contains('light')).toBe(false);
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('dark');

    fireEvent.click(screen.getByText('toggle'));
    expect(screen.getByTestId('appearance').textContent).toBe('light');
    expect(root().classList.contains('light')).toBe(true);
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('light');
  });

  it('persists across a reload (fresh mount reads the saved value)', () => {
    const first = render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    fireEvent.click(screen.getByText('set-dark'));
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('dark');
    first.unmount();

    render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    expect(screen.getByTestId('appearance').textContent).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
  });

  it('works in-memory when storage is blocked', () => {
    vi.spyOn(Storage.prototype, 'setItem').mockImplementation(() => {
      throw new Error('blocked');
    });
    render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    expect(() => fireEvent.click(screen.getByText('set-dark'))).not.toThrow();
    expect(screen.getByTestId('appearance').textContent).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
  });

  it('useAppearance throws outside a provider', () => {
    const spy = vi.spyOn(console, 'error').mockImplementation(() => undefined);
    expect(() => render(<Probe />)).toThrow(
      /within an AppTheme\/ThemeProvider/,
    );
    spy.mockRestore();
  });

  it('a nested provider reuses the parent (single source of truth)', () => {
    render(
      <ThemeProvider>
        <ThemeProvider>
          <Probe />
        </ThemeProvider>
      </ThemeProvider>,
    );
    // exactly one appearance class, driven through the shared context
    fireEvent.click(screen.getByText('set-dark'));
    expect(screen.getByTestId('appearance').textContent).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
    expect(root().classList.contains('light')).toBe(false);
  });

  it('restores the prior root appearance + color-scheme on unmount, keeping unrelated classes', () => {
    const el = root();
    // Prior state (e.g. the bootstrap-applied class) + an unrelated app class.
    el.classList.add('some-app-class', 'dark');
    el.style.colorScheme = 'dark';
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'light');

    const { unmount } = render(
      <ThemeProvider>
        <Probe />
      </ThemeProvider>,
    );
    // provider applied the stored light appearance...
    expect(el.classList.contains('light')).toBe(true);
    expect(el.classList.contains('dark')).toBe(false);

    unmount();
    // ...and on unmount restored the prior dark class + color-scheme.
    expect(el.classList.contains('dark')).toBe(true);
    expect(el.classList.contains('light')).toBe(false);
    expect(el.style.colorScheme).toBe('dark');
    // the unrelated class was never touched.
    expect(el.classList.contains('some-app-class')).toBe(true);

    el.classList.remove('some-app-class', 'dark');
  });

  it('is StrictMode-safe: no stray write on mount, one write per real change', () => {
    render(
      <StrictMode>
        <ThemeProvider>
          <Probe />
        </ThemeProvider>
      </StrictMode>,
    );
    // default light, read (not written) -> storage stays empty through the
    // StrictMode mount/unmount/remount cycle.
    expect(screen.getByTestId('appearance').textContent).toBe('light');
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBeNull();

    fireEvent.click(screen.getByText('set-dark'));
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('dark');
    expect(root().classList.contains('dark')).toBe(true);
  });
});
