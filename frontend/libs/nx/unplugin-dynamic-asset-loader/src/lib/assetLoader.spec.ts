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
      `<script src="runtime.esm.js" type="module" /><link rel="stylesheet" href="styles.css"/>`
    );

    expect(result.js).toHaveLength(1);
    expect(result.js[0].jsModule).toBeTruthy();
  });

  it("should detect correctly if it's not a js module", () => {
    const result = extractAssets(
      `<script src="runtime.esm.js"/><link rel="stylesheet" href="styles.css"/>`
    );

    expect(result.js).toHaveLength(1);
    expect(result.js[0].jsModule).toBeFalsy();
  });

  it('should throw when there is no css assets in the html', () => {
    expect(() => extractAssets(`<script src="runtime.esm.js"/>`)).toThrowError(
      'No css assets found, there is an issue with the provided html.'
    );
  });

  it('should throw when there is no js assets in the html', () => {
    expect(() =>
      extractAssets(`<link rel="stylesheet" href="styles.css"/>`)
    ).toThrowError(
      'No js assets found, there is an issue with the provided html.'
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
      })
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
