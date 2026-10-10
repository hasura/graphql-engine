// jest-dom adds custom jest matchers for asserting on DOM nodes.
// allows you to do things like:
// expect(element).toHaveTextContent(/react/i)
// learn more: https://github.com/testing-library/jest-dom

import { vi } from 'vitest';
import { TextDecoder, TextEncoder } from 'util';

import '@testing-library/jest-dom/vitest';

// Intentionally mock the Lottie animation at its leaf (`lottie-react`): it loads
// `lottie-web`, which probes a 2D canvas context at import time (jsdom has none)
// and crashes every suite that transitively imports `LoadingScreen` via the shared
// UI barrel. Stubbing the animation component is the minimal, explicit boundary —
// real `HTMLCanvasElement.getContext` is left untouched (non-Lottie canvas stays
// unavailable; no global no-op/Proxy). Animation rendering is intentionally not
// exercised in unit tests.
vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

if (typeof global.TextEncoder === 'undefined') {
  Object.assign(global, { TextEncoder, TextDecoder });
}

Object.assign(window, {
  __env: {
    consoleType: 'oss',
    nodeEnv: 'development',
    serverVersion: 'v1.0.0',
  },
});
