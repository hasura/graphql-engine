const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

function setup({ fail = false } = {}) {
  const created = [];
  const revoked = [];
  class NativeWorker extends EventTarget {
    constructor(url, options) {
      super();
      if (fail) throw new Error('worker blocked');
      this.url = String(url);
      this.options = options;
    }
    terminate() {
      this.terminated = true;
    }
  }
  class MockURL extends URL {
    static createObjectURL(blob) {
      created.push(blob);
      return 'blob:https://console.test/worker';
    }
    static revokeObjectURL(url) {
      revoked.push(url);
    }
  }
  const context = vm.createContext({
    Worker: NativeWorker,
    URL: MockURL,
    Blob,
    location: {
      href: 'https://console.test/console/',
      origin: 'https://console.test',
    },
    module: { exports: {} },
  });
  vm.runInContext(
    fs.readFileSync(require.resolve('./monaco-cdn-worker'), 'utf8'),
    context,
  );
  return { Worker: context.module.exports.MonacoCdnWorker, created, revoked };
}

test('same-origin workers use their original URL and options without a blob', () => {
  const { Worker, created, revoked } = setup();
  const worker = new Worker('/versioned/editor.js', { name: 'editor' });
  assert.equal(worker.url, 'https://console.test/versioned/editor.js');
  assert.equal(worker.options.name, 'editor');
  worker.terminate();
  assert.equal(worker.terminated, true);
  assert.equal(created.length, 0);
  assert.equal(revoked.length, 0);
});

test('CDN bootstrap records the full entry URL and imports it as a classic worker', async () => {
  const { Worker, created, revoked } = setup();
  const url = 'https://cdn.test/assets/versioned/v1/graphql.js?version=1';
  const worker = new Worker(url);
  assert.equal(worker.url, 'blob:https://console.test/worker');
  assert.equal(created[0].type, 'text/javascript');
  assert.equal(
    await created[0].text(),
    `self.__HASURA_MONACO_WORKER_URL__ = ${JSON.stringify(url)};\nimportScripts(${JSON.stringify(url)});`,
  );
  assert.equal(revoked.length, 0, 'do not revoke before the worker loads');
  worker.dispatchEvent(new Event('message'));
  worker.dispatchEvent(new Event('message'));
  worker.terminate();
  assert.equal(revoked.length, 1, 'release only once');
});

for (const event of ['error', 'terminate']) {
  test(`CDN bootstrap is released on ${event} before initialization`, () => {
    const { Worker, revoked } = setup();
    const worker = new Worker('https://cdn.test/editor.js');
    if (event === 'terminate') worker.terminate();
    else worker.dispatchEvent(new Event('error'));
    worker.terminate();
    assert.equal(revoked.length, 1);
  });
}

test('constructor failure releases the blob and preserves the error', () => {
  const { Worker, revoked } = setup({ fail: true });
  assert.throws(
    () => new Worker('https://cdn.test/editor.js'),
    /worker blocked/,
  );
  assert.equal(revoked.length, 1);
});
