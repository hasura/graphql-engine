// Jest's CLI always forced `NODE_ENV=test` when it wasn't already set.
// Vitest only does the equivalent `??=`, so it never overrides a value
// that Nx's dotenv loading already populated from the repo's `.env`
// (which sets `NODE_ENV=development`). Force it back to `test` here so
// env-dependent code (e.g. feature flags gated on NODE_ENV) behaves the
// same under Vitest as it did under Jest.
process.env.NODE_ENV = 'test';

import { vi } from 'vitest';

// jest-dom adds custom jest matchers for asserting on DOM nodes.
// allows you to do things like:
// expect(element).toHaveTextContent(/react/i)
// learn more: https://github.com/testing-library/jest-dom
// eslint-disable-next-line import/no-extraneous-dependencies
import '@testing-library/jest-dom/vitest';

// Intentionally mock the Lottie animation at its leaf (`lottie-react`). It loads
// `lottie-web`, whose `ImagePreloader` probes a 2D canvas context at *import* time
// — which jsdom doesn't implement — and crashes every suite that transitively
// imports `LoadingScreen` via the shared UI barrel. Stubbing the animation
// component (rendered as nothing) is the minimal, explicit boundary: it does NOT
// touch `HTMLCanvasElement.getContext`, so real canvas behaviour stays unavailable
// for any non-Lottie code (no global no-op/Proxy masking). Animation *rendering* is
// intentionally not exercised in unit tests.
vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

// jsdom doesn't implement ResizeObserver, but Radix UI components
// (e.g. Select's scroll area) rely on it being present at mount time.
if (typeof globalThis.ResizeObserver === 'undefined') {
  globalThis.ResizeObserver = class ResizeObserver {
    observe() {}

    unobserve() {}

    disconnect() {}
  };
}
