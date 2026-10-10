import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  APPEARANCE_STORAGE_KEY,
  isAppearance,
  readStoredAppearance,
  resolveInitialAppearance,
  writeStoredAppearance,
} from './appearance';

afterEach(() => {
  window.localStorage.clear();
  vi.restoreAllMocks();
});

describe('appearance storage', () => {
  it('isAppearance validates the union', () => {
    expect(isAppearance('light')).toBe(true);
    expect(isAppearance('dark')).toBe(true);
    expect(isAppearance('system')).toBe(false);
    expect(isAppearance('')).toBe(false);
    expect(isAppearance(null)).toBe(false);
    expect(isAppearance(undefined)).toBe(false);
  });

  it('returns null when nothing is stored', () => {
    expect(readStoredAppearance()).toBeNull();
  });

  it('reads a valid stored value', () => {
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'dark');
    expect(readStoredAppearance()).toBe('dark');
  });

  it('treats a malformed stored value as absent', () => {
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'neon');
    expect(readStoredAppearance()).toBeNull();
  });

  it('round-trips through write/read', () => {
    writeStoredAppearance('dark');
    expect(window.localStorage.getItem(APPEARANCE_STORAGE_KEY)).toBe('dark');
    expect(readStoredAppearance()).toBe('dark');
  });

  it('does not throw and reads null when getItem is blocked', () => {
    vi.spyOn(Storage.prototype, 'getItem').mockImplementation(() => {
      throw new Error('storage blocked');
    });
    expect(() => readStoredAppearance()).not.toThrow();
    expect(readStoredAppearance()).toBeNull();
  });

  it('does not throw when setItem is blocked (private mode)', () => {
    vi.spyOn(Storage.prototype, 'setItem').mockImplementation(() => {
      throw new Error('storage blocked');
    });
    expect(() => writeStoredAppearance('dark')).not.toThrow();
  });

  it('resolveInitialAppearance defaults to light, else the stored value', () => {
    expect(resolveInitialAppearance()).toBe('light');
    window.localStorage.setItem(APPEARANCE_STORAGE_KEY, 'dark');
    expect(resolveInitialAppearance()).toBe('dark');
  });
});
