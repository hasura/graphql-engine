import {
  extractAssets,
  generateAssetLoaderFile,
  generateDynamicLoadCalls,
} from './assetLoader';

const exampleHtml = `
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>ConsoleCe</title>
    <base href="/">
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link rel="icon" type="image/x-icon" href="favicon.ico" />
    <script src="https://graphql-engine-cdn.hasura.io/pro-console/assets/common/js/lottie.min.js"></script>
    <script>
      window.__env = envVars;

    </script>
  <link rel="stylesheet" href="styles.css"></head>
  <body>
  <style>
    .content {
      display: none;
      opacity: 0;
      transition: opacity 0.2s linear;
    }
  </style>
    <script
      src="https://graphql-engine-cdn.hasura.io/cloud-console/assets/common/wasm/go1.16/wasm_exec.js"
      charset="UTF-8"
    ></script>
  <script src="runtime.esm.js" type="module"></script><script src="polyfills.esm.js" type="module"></script><script src="vendor.esm.js" type="module"></script><script src="main.esm.js" type="module"></script></body>
</html>
`;

describe('extractAssets', () => {
  it('should be able to extract css tags', () => {
    const result = extractAssets(exampleHtml);

    expect(result.css).toHaveLength(1);
    expect(result.css[0]).toEqual({
      type: 'css',
      tag: '<link rel="stylesheet" href="styles.css"/>',
      url: 'styles.css',
    });
  });
  it('should be able to extract all javascript tags that load files', () => {
    const result = extractAssets(exampleHtml);

    expect(result.js).toHaveLength(4);
    expect(result.js).toMatchInlineSnapshot(`
      [
        {
          "jsModule": true,
          "tag": "<script src="runtime.esm.js" type="module"></script>",
          "type": "js",
          "url": "runtime.esm.js",
        },
        {
          "jsModule": true,
          "tag": "<script src="polyfills.esm.js" type="module"></script>",
          "type": "js",
          "url": "polyfills.esm.js",
        },
        {
          "jsModule": true,
          "tag": "<script src="vendor.esm.js" type="module"></script>",
          "type": "js",
          "url": "vendor.esm.js",
        },
        {
          "jsModule": true,
          "tag": "<script src="main.esm.js" type="module"></script>",
          "type": "js",
          "url": "main.esm.js",
        },
      ]
    `);
  });

  it("should detect correctly if it's a js module", () => {
    const result = extractAssets(
      `<script src="runtime.esm.js" type="module" /><link rel="stylesheet" href="styles.css"/>`,
    );

    expect(result.js).toHaveLength(1);
    expect(result.js[0].jsModule).toBeTruthy();
  });

  it("should detect correctly if it's not a js module", () => {
    const result = extractAssets(
      `<script src="runtime.esm.js"/><link rel="stylesheet" href="styles.css"/>`,
    );

    expect(result.js).toHaveLength(1);
    expect(result.js[0].jsModule).toBeFalsy();
  });

  // Contract guard for the real webpack-generated index.html shape: a large inline
  // env-var <script>, multiple content-hashed CSS files and several `type="module"`
  // chunk scripts. (The actual `dom-parser` crash — `Cannot read properties of null
  // (reading 'isSelfCloseTag')` — was reproduced end-to-end by the
  // build-server-assets executor, see bsa-ce-baseline-raw.log; it does not surface
  // in a standalone dom-parser harness, so this guards the htmlparser2 output
  // contract rather than re-creating the executor-runtime crash.)
  const webpackIndexHtml = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <base href="/">
    <script>
      const getEnv = (value) =>
        typeof value !== 'string' || value.startsWith('%NX') ? undefined : value;
      const serverEnvVars = { dataApiUrl: getEnv('%NX_PUBLIC_DATA_API_URL%') };
      window.__env = serverEnvVars;
    </script>
  <link rel="stylesheet" href="styles.7fad8c9358a8cf41.css"><link rel="stylesheet" href="vendor.28a252c37d4c7767.css"><link rel="stylesheet" href="main.da56d508139ca103.css"></head>
  <body>
    <style>.content { display: none; opacity: 0; }</style>
    <div id="content" class="content"></div>
  <script src="runtime.8d9577ec68952afd.js" type="module"></script><script src="polyfills.d4a952a4e487fcc4.js" type="module"></script><script src="styles.e391e20e6de41eec.js" type="module"></script><script src="vendor.e651d5d64de6dee4.js" type="module"></script><script src="main.7d765d61cddaa18b.js" type="module"></script></body>
</html>`;

  it('parses the real webpack index.html (inline env script + hashed module chunks)', () => {
    const result = extractAssets(webpackIndexHtml);

    expect(result.css.map((c) => c.url)).toEqual([
      'styles.7fad8c9358a8cf41.css',
      'vendor.28a252c37d4c7767.css',
      'main.da56d508139ca103.css',
    ]);
    expect(result.js.map((j) => j.url)).toEqual([
      'runtime.8d9577ec68952afd.js',
      'polyfills.d4a952a4e487fcc4.js',
      'styles.e391e20e6de41eec.js',
      'vendor.e651d5d64de6dee4.js',
      'main.7d765d61cddaa18b.js',
    ]);
    // every chunk is an ES module, and the inline env <script> (no src) is ignored
    expect(result.js.every((j) => j.jsModule)).toBe(true);
  });

  it('parses a minified single-line document with void tags', () => {
    // A minified index.html: no whitespace, void elements (<meta>, <base>, <link>)
    // left unclosed, module chunk scripts back-to-back — the shape the previous
    // parser choked on.
    const minified =
      `<!doctype html><html><head><meta charset="utf-8"><base href="/">` +
      `<link rel="icon" href="favicon.ico"><link rel="stylesheet" href="main.abc123.css">` +
      `<script>window.__env={a:1>0?1:2};</script></head><body>` +
      `<script src="runtime.aaa.js" type="module"></script><script src="main.bbb.js" type="module"></script>` +
      `</body></html>`;
    const result = extractAssets(minified);
    expect(result.css.map((c) => c.url)).toEqual(['main.abc123.css']);
    expect(result.js.map((j) => j.url)).toEqual([
      'runtime.aaa.js',
      'main.bbb.js',
    ]);
    // the favicon <link> (rel=icon) is ignored; only stylesheets count as css
    expect(result.css).toHaveLength(1);
  });

  it('should throw when there is no css assets in the html', () => {
    expect(() => extractAssets(`<script src="runtime.esm.js"/>`)).toThrowError(
      'No css assets found, there is an issue with the provided html.',
    );
  });

  it('should throw when there is no js assets in the html', () => {
    expect(() =>
      extractAssets(`<link rel="stylesheet" href="styles.css"/>`),
    ).toThrowError(
      'No js assets found, there is an issue with the provided html.',
    );
  });
});

describe('generateDynamicLoadCalls', () => {
  it('should generate the css loader', () => {
    const result = generateDynamicLoadCalls({
      css: [
        {
          url: 'todo.css',
          tag: '',
          type: 'css',
        },
      ],
      js: [],
    });

    expect(result).toEqual('loadCss(basePath + "todo.css");\n');
  });
  it('should generate the css in the same order as the dom', () => {
    const result = generateDynamicLoadCalls({
      css: [
        {
          url: 'todo.css',
          tag: '',
          type: 'css',
        },
        {
          url: 'my.css',
          tag: '',
          type: 'css',
        },
      ],
      js: [],
    });

    expect(result).toEqual(`loadCss(basePath + "todo.css");
loadCss(basePath + "my.css");
`);
  });

  it('should generate the js loader', () => {
    const result = generateDynamicLoadCalls({
      js: [
        {
          url: 'todo.js',
          tag: '',
          type: 'js',
          jsModule: false,
        },
      ],
      css: [],
    });

    expect(result).toEqual('loadJs(basePath + "todo.js");\n');
  });
  it('should generate the js loader with support for js modules', () => {
    const result = generateDynamicLoadCalls({
      js: [
        {
          url: 'todo.js',
          tag: '',
          type: 'js',
          jsModule: true,
        },
      ],
      css: [],
    });

    expect(result).toEqual('loadJs(basePath + "todo.js", "module");\n');
  });
  it('should generate the js loader in the same order as the dom', () => {
    const result = generateDynamicLoadCalls({
      js: [
        {
          url: 'todo.js',
          tag: '',
          type: 'js',
          jsModule: true,
        },
        {
          url: 'my.js',
          tag: '',
          type: 'js',
          jsModule: false,
        },
      ],
      css: [],
    });

    expect(result).toEqual(`loadJs(basePath + "todo.js", "module");
loadJs(basePath + "my.js");
`);
  });

  it('should combine the css and js in the correct order', () => {
    const result = generateDynamicLoadCalls({
      js: [
        {
          url: 'todo.js',
          tag: '',
          type: 'js',
          jsModule: true,
        },
        {
          url: 'my.js',
          tag: '',
          type: 'js',
          jsModule: false,
        },
      ],
      css: [
        {
          url: 'my.css',
          tag: '',
          type: 'css',
        },
      ],
    });

    expect(result).toEqual(`loadCss(basePath + "my.css");
loadJs(basePath + "todo.js", "module");
loadJs(basePath + "my.js");
`);
  });
});

describe('generateAssetLoaderFile', () => {
  it('should return the full js file content', () => {
    expect(
      generateAssetLoaderFile({
        js: [
          {
            url: 'todo.js',
            tag: '',
            type: 'js',
            jsModule: true,
          },
          {
            url: 'my.js',
            tag: '',
            type: 'js',
            jsModule: false,
          },
        ],

        css: [
          {
            url: 'my.css',
            tag: '',
            type: 'css',
          },
        ],
      }),
    ).toMatchInlineSnapshot(`
      "// THIS FILE IS GENERATED; DO NOT MODIFY BY HAND.

      const loadCss = (url) => {
        const linkElem = document.createElement("link");
        linkElem.rel = "stylesheet";
        linkElem.charset = "UTF-8";
        linkElem.href = url;
        document.body.append(linkElem);
      };
      const loadJs = (url, type) => {
        const scriptElem = document.createElement("script");
        scriptElem.charset = "UTF-8";
        scriptElem.src = url;
        if (type) {
          scriptElem.type = type
        }
        document.body.append(scriptElem);
      };

      window.__loadConsoleAssetsFromBasePath = (root) => {
      const basePath = root.endsWith('/') ? root : root + '/';
      loadCss(basePath + "my.css");
      loadJs(basePath + "todo.js", "module");
      loadJs(basePath + "my.js");
      }"
    `);
  });
});
