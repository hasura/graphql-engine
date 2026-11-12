// Run: node --test tools/webpack/monaco-workers.browser.test.cjs
// Compiles the official GraphiQL worker setup and exercises real Monaco RPCs
// with app/assets on both the same origin and separate origins (CDN deployment).
const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs/promises');
const path = require('node:path');
const os = require('node:os');
const http = require('node:http');
const webpack = require('webpack');
// Reuse the browser tooling already installed for the Storybook test runner.
const { createRequire } = require('node:module');
const { existsSync } = require('node:fs');
const storybookRequire = createRequire(
  require.resolve('@storybook/test-runner'),
);
const { chromium } = storybookRequire('playwright');
const {
  MonacoWorkerPublicPathPlugin,
} = require('./monaco-worker-public-path-plugin');

const root = path.resolve(__dirname, '../..');
const setupPath = path.join(
  path.dirname(require.resolve('@graphiql/react/package.json')),
  'dist/setup-workers/webpack.js',
);

const entry = `
import ${JSON.stringify(setupPath)};
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api.js';
import 'monaco-editor/esm/vs/language/json/monaco.contribution.js';
window.mountExplorer = async () => {
  const jsonModel = monaco.editor.createModel('{"broken": }', 'json');
  const graphqlModel = monaco.editor.createModel('query {', 'graphql');
  const graphqlWorker = monaco.editor.createWebWorker({
    moduleId: 'monaco-graphql/esm/GraphQLWorker.js', label: 'graphql',
    createData: { languageConfig: { schemas: [{ uri: 'schema.graphql', documentString: 'type Query { hello: String }' }] }, formattingOptions: {} }
  });
  const jsonWorker = monaco.editor.createWebWorker({
    moduleId: 'vs/language/json/jsonWorker', label: 'json',
    createData: { languageSettings: { validate: true } }
  });
  const diff = monaco.editor.createDiffEditor(document.getElementById('editor'), {
    automaticLayout: true
  });
  const original = monaco.editor.createModel('original', 'plaintext');
  const modified = monaco.editor.createModel('modified', 'plaintext');
  diff.setModel({ original, modified });
  const diffReady = new Promise(resolve => {
    const listener = diff.onDidUpdateDiff(() => {
      listener.dispose(); resolve(diff.getLineChanges().length);
    });
  });
  const [json, graphql] = await Promise.all([
    jsonWorker.withSyncedResources([jsonModel.uri]).then(w => w.doValidation(jsonModel.uri.toString())),
    graphqlWorker.withSyncedResources([graphqlModel.uri]).then(w => w.doValidation(graphqlModel.uri.toString()))
  ]);
  const changes = await diffReady;
  window.leaveExplorer = () => {
    const start = performance.now();
    diff.dispose(); graphqlWorker.dispose(); jsonWorker.dispose();
    for (const model of [jsonModel, graphqlModel, original, modified]) model.dispose();
    history.pushState({}, '', '/console/data');
    return performance.now() - start;
  };
  return { jsonDiagnostics: json.length, graphqlDiagnostics: graphql.length, changes };
};
window.ready = true;
`;

function listen(server) {
  return new Promise((resolve) =>
    server.listen(0, '127.0.0.1', () => {
      resolve(`http://127.0.0.1:${server.address().port}`);
    }),
  );
}
function close(server) {
  return new Promise((resolve) => server.close(resolve));
}
function compile(config) {
  return new Promise((resolve, reject) => {
    const compiler = webpack(config);
    compiler.run((error, stats) => {
      compiler.close(() => {
        if (error) reject(error);
        else if (stats.hasErrors())
          reject(new Error(stats.toString({ all: false, errors: true })));
        else resolve(stats);
      });
    });
  });
}

test(
  'GraphiQL JSON, GraphQL and editor workers survive CDN origins and route teardown',
  { timeout: 180000 },
  async () => {
    const temp = await fs.mkdtemp(path.join(os.tmpdir(), 'monaco-cdn-'));
    const output = path.join(temp, 'dist');
    await fs.writeFile(path.join(temp, 'entry.js'), entry);
    const stats = await compile({
      context: root,
      mode: 'development',
      entry: path.join(temp, 'entry.js'),
      devtool: false,
      resolve: { modules: [path.join(root, 'node_modules'), 'node_modules'] },
      module: { rules: [{ test: /\.css$/, type: 'asset/source' }] },
      output: {
        path: output,
        filename: '[name].js',
        chunkFilename: '[id].js',
        publicPath: 'auto',
        scriptType: 'module',
        module: false,
      },
      optimization: {
        splitChunks: { chunks: 'all', minSize: 10000 },
        runtimeChunk: false,
      },
      plugins: [new MonacoWorkerPublicPathPlugin()],
    });
    const chunks = stats
      .toJson({ all: false, entrypoints: true })
      .entrypoints.main.assets.map((a) => a.name);
    let assetOrigin;
    const requests = [];
    const app = http.createServer((req, res) => {
      const scripts = chunks
        .filter((c) => c.endsWith('.js'))
        .map(
          (c) =>
            `<script type="module" src="${assetOrigin}/assets/versioned/test/${c}"></script>`,
        )
        .join('');
      res.setHeader('Content-Type', 'text/html');
      res.end(
        `<div id="editor" style="width:800px;height:400px"></div>${scripts}`,
      );
    });
    const assets = http.createServer(async (req, res) => {
      requests.push(req.url);
      res.setHeader('Access-Control-Allow-Origin', '*');
      const name = req.url.replace('/assets/versioned/test/', '');
      try {
        const bytes = await fs.readFile(path.join(output, path.basename(name)));
        res.setHeader('Content-Type', 'text/javascript');
        res.end(bytes);
      } catch {
        res.statusCode = 404;
        res.end('not found');
      }
    });
    const appOrigin = await listen(app);
    const cdnOrigin = await listen(assets);
    let browser;
    try {
      browser = await chromium.launch({
        executablePath:
          process.env.CHROME_BIN ||
          (existsSync('/usr/bin/google-chrome')
            ? '/usr/bin/google-chrome'
            : undefined),
        headless: true,
        args: ['--no-sandbox'],
      });
      // Same-origin mode proxies asset requests through the application server.
      const handleApp = app.listeners('request')[0];
      app.removeAllListeners('request');
      app.on('request', (req, res) => {
        if (req.url.startsWith('/assets/')) assets.emit('request', req, res);
        else handleApp(req, res);
      });
      for (const crossOrigin of [false, true]) {
        assetOrigin = crossOrigin ? cdnOrigin : appOrigin;
        const page = await browser.newPage();
        const errors = [];
        const workers = [];
        page.on('pageerror', (error) => errors.push(error.message));
        page.on('console', (msg) => {
          if (
            msg.type() === 'error' ||
            /Could not create web worker/.test(msg.text())
          )
            errors.push(msg.text());
        });
        page.on('worker', (worker) => workers.push(worker.url()));
        await page.goto(appOrigin + '/console/api/api-explorer');
        await page.waitForFunction(() => window.ready, { timeout: 30000 });
        // The browser's global constructor must remain native. This is also a
        // negative control: the original direct-CDN construction really fails
        // even though this asset server sends Access-Control-Allow-Origin: *.
        if (crossOrigin) {
          assert.equal(
            await page.evaluate((url) => {
              try {
                const worker = new Worker(url);
                worker.terminate();
                return 'unexpected success';
              } catch (error) {
                return error.name;
              }
            }, `${cdnOrigin}/assets/versioned/test/unused.js`),
            'SecurityError',
          );
        }
        const durations = [];
        for (let i = 0; i < 3; i++) {
          const result = await page.evaluate(() => window.mountExplorer());
          assert.ok(result.jsonDiagnostics > 0, 'JSON validation really ran');
          assert.ok(
            result.graphqlDiagnostics > 0,
            'GraphQL validation really ran',
          );
          assert.ok(result.changes > 0, 'editor diff worker really ran');
          durations.push(await page.evaluate(() => window.leaveExplorer()));
        }
        assert.ok(
          workers.length >= 7,
          `all worker types constructed: ${workers}`,
        );
        assert.ok(
          workers.every((url) =>
            crossOrigin ? url.startsWith('blob:') : !url.startsWith('blob:'),
          ),
        );
        assert.deepEqual(errors, []);
        assert.ok(
          requests.every((url) => url.startsWith('/assets/versioned/test/')),
          'split worker chunks retain the CDN base path',
        );
        console.log(
          JSON.stringify({
            crossOrigin,
            workers: workers.length,
            teardownMs: durations,
            errors,
          }),
        );
        await page.close();
      }
    } finally {
      await browser?.close();
      await Promise.all([close(app), close(assets)]);
      await fs.rm(temp, { recursive: true, force: true });
    }
  },
);
