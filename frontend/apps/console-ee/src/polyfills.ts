/**
 * Polyfill stable language features. These imports will be optimized by `@babel/preset-env`.
 *
 * See: https://github.com/zloirock/core-js#babel
 */
import 'core-js/stable';
import 'regenerator-runtime/runtime';
// Needed for action codegen
import 'browser-hrtime';
import { Buffer } from 'buffer/';
process.cwd = () => '';

(window as any).global = window;
(window as any).Buffer = Buffer;
