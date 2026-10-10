// @ts-nocheck
// Forces WORKER chunks to use webpack's classic (self.location) public-path
// detection instead of `import.meta.url`.
//
// Why this is needed (see also GraphiQLWrapper/setupMonacoWorkers rationale):
// Nx 23 defaults `output.scriptType: 'module'` for web targets while leaving
// `output.module: false` (apps/*/webpack build emits classic array-push chunks
// served as `<script type="module">`). webpack's AutoPublicPathRuntimeModule
// keys its detection off the GLOBAL `scriptType`, so EVERY chunk's public-path
// runtime is emitted as `if (typeof import.meta.url === "string") ...`. That is
// valid in the main thread (module scripts) and is genuinely required there
// (e.g. prettier's ESM plugins use `import.meta`), so we must NOT turn
// `scriptType` off globally.
//
// Monaco's workers, however, are spawned by GraphiQL as CLASSIC workers (webpack
// rewrites `new Worker(..., { type })` to `type: undefined` because
// `output.module` is false), and a classic worker whose bundle contains the
// `import.meta` token fails to even parse:
//   "Cannot use 'import.meta' outside a module"
// after which Monaco silently drops its language services to the main thread.
//
// This plugin rewrites ONLY the public-path runtime of worker chunks to the
// `self.location`-based detection webpack already uses for non-module output.
// That keeps the worker auto-detecting its own URL (so nested / sub-path
// deployments and the worker's own lazy chunk loads keep working) with no
// `import.meta` token. Main-thread chunks are left untouched.
const RuntimeGlobals = require('webpack/lib/RuntimeGlobals');
const Template = require('webpack/lib/Template');

const PLUGIN = 'MonacoWorkerPublicPathPlugin';

/** Is this chunk the runtime of a web-worker entrypoint? */
function isWorkerChunk(chunk) {
  const entryOptions = chunk.getEntryOptions();
  return Boolean(entryOptions && entryOptions.worker);
}

/**
 * The classic, `self.location`-based public-path detection. This mirrors the
 * worker branch of webpack's own AutoPublicPathRuntimeModule (`self.location` is
 * the worker's own script URL; strip the filename to get its directory, then
 * `undoPath` walks back to the output root), with NO `import.meta` token.
 *
 * `self` is used directly (not `__webpack_require__.g`): this runtime is swapped
 * in after webpack has already computed the chunk's runtime requirements, so the
 * `global` helper may not be present — but these chunks always execute in a
 * worker scope, where `self` is defined.
 */
function classicPublicPathSource(compilation, chunk) {
  const runtimeTemplate = compilation.runtimeTemplate;
  const undoPath = runtimeTemplate.chunkRootOutputDir(chunk, false);
  const lt = runtimeTemplate.renderLet();

  return Template.asString([
    `${lt} scriptUrl;`,
    'if (typeof self !== "undefined" && self.location) scriptUrl = self.location.href;',
    'if (!scriptUrl) throw new Error("Automatic publicPath is not supported in this worker");',
    'scriptUrl = scriptUrl.replace(/^blob:|[?#].*$/g, "").replace(/\\/[^/]+$/, "/");',
    !undoPath
      ? `${RuntimeGlobals.publicPath} = scriptUrl;`
      : `${RuntimeGlobals.publicPath} = scriptUrl + ${JSON.stringify(undoPath)};`,
  ]);
}

class MonacoWorkerPublicPathPlugin {
  apply(compiler) {
    compiler.hooks.compilation.tap(PLUGIN, (compilation) => {
      compilation.hooks.runtimeModule.tap(PLUGIN, (module, chunk) => {
        if (module.name !== 'publicPath') return;
        if (!isWorkerChunk(chunk)) return;
        // Replace the generated source with the classic detection. `generate()`
        // runs later (at code-generation), so overriding it here wins.
        module.generate = () => classicPublicPathSource(compilation, chunk);
      });
    });
  }
}

module.exports = { MonacoWorkerPublicPathPlugin };
