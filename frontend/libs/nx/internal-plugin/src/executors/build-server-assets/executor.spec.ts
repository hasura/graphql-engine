import {
  generatePolyfillLoaderFile,
  gzAssetNames,
  validateAllowedAssets,
} from './executor';
import { generateAssetLoaderFile } from 'unplugin-dynamic-asset-loader';

describe('validateAllowedAssets', () => {
  for (const forbiddenAsset of ['main.css', 'main.js', 'vendor.js']) {
    it(`should reject a js asset with the name ${forbiddenAsset}`, () => {
      expect(() =>
        validateAllowedAssets({
          js: [{ jsModule: false, type: 'js', tag: '', url: forbiddenAsset }],
          css: [],
        })
      ).toThrowError(
        `Cannot use ${forbiddenAsset}: This file cannot be used anymore give it was used prior to 2.18 and we can't use that file name anymore since cli may load this instead of the correct assets.`
      );
    });
    it(`should reject a css asset with the name ${forbiddenAsset}`, () => {
      expect(() =>
        validateAllowedAssets({
          css: [{ type: 'css', tag: '', url: forbiddenAsset }],
          js: [],
        })
      ).toThrowError(
        `Cannot use ${forbiddenAsset}: This file cannot be used anymore give it was used prior to 2.18 and we can't use that file name anymore since cli may load this instead of the correct assets.`
      );
    });
  }

  it('should allow for the rest of the files', () => {
    expect(() =>
      validateAllowedAssets({
        js: [{ url: 'something.js', tag: '', jsModule: true, type: 'js' }],
        css: [{ url: 'something.css', type: 'css', tag: '' }],
      })
    ).not.toThrow();
  });
});

describe('gzAssetNames', () => {
  it('should add gz prefix to assets', () => {
    expect(
      gzAssetNames({
        js: [{ url: 'my.js', tag: '', type: 'js', jsModule: false }],
        css: [{ url: 'my.css', tag: '', type: 'css' }],
      })
    ).toEqual({
      js: [{ url: 'my.js.gz', tag: '', type: 'js', jsModule: false }],
      css: [{ url: 'my.css.gz', tag: '', type: 'css' }],
    });
  });
});

describe('generatePolyfillLoaderFile', () => {
  it('should return the full js file content', () => {
    expect(
      generatePolyfillLoaderFile(
        generateAssetLoaderFile({
          js: [
            {
              url: 'todo.js.gz',
              tag: '',
              type: 'js',
              jsModule: true,
            },
            {
              url: 'my.js.gz',
              tag: '',
              type: 'js',
              jsModule: false,
            },
          ],

          css: [
            {
              url: 'my.css.gz',
              tag: '',
              type: 'css',
            },
          ],
        })
      )
    ).toMatchInlineSnapshot(`
      "// This file exists for older cli using newer asset names.
      // THIS FILE IS GENERATED; DO NOT MODIFY BY HAND.

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
      loadCss(basePath + "my.css.gz");
      loadJs(basePath + "todo.js.gz", "module");
      loadJs(basePath + "my.js.gz");
      }

      // This is from the old console template for the CLI
      window.__loadConsoleAssetsFromBasePath(window.__env.versionedAssetsPath);

      console.log('Please note that the error of loading vendor.js and main.css is normal.');

      "
    `);
  });
});
