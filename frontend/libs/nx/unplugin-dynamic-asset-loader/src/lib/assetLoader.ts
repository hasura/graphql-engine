import DomParser from 'dom-parser';

export type Asset = {
  tag: string;
  url: string;
};

export type JsAsset = Asset & {
  type: 'js';
  jsModule?: boolean;
};

export type CssAsset = Asset & {
  type: 'css';
};

export type Assets = {
  js: JsAsset[];
  css: CssAsset[];
};

export const extractAssets = (html: string): Assets => {
  const assets: Assets = { js: [], css: [] };
  const domParser = new DomParser();
  const parsedDocument = domParser.parseFromString(html);
  parsedDocument.getElementsByTagName('script')?.forEach(element => {
    if (
      element.getAttribute('src') &&
      !element.getAttribute('src')?.startsWith('http')
    ) {
      assets.js.push({
        tag: element.outerHTML,
        url: element.getAttribute('src') || 'not_found',
        jsModule: element.getAttribute('type') === 'module',
        type: 'js',
      });
    }
  });
  parsedDocument
    .getElementsByAttribute('rel', 'stylesheet')
    ?.forEach(element => {
      if (
        element.getAttribute('href') &&
        !element.getAttribute('href')?.startsWith('http')
      ) {
        assets.css.push({
          tag: element.outerHTML,
          url: element.getAttribute('href') || 'not_found',
          type: 'css',
        });
      }
    });

  if (assets.css.length === 0) {
    throw new Error(
      'No css assets found, there is an issue with the provided html.'
    );
  }
  if (assets.js.length === 0) {
    throw new Error(
      'No js assets found, there is an issue with the provided html.'
    );
  }
  return assets;
};

export const generateDynamicLoadCalls = (assets: Assets): string => {
  const cssMap = assets.css
    .map(it => `loadCss(basePath + "${it.url}");\n`)
    .join('');

  const jsMap = assets.js
    .map(it => {
      if (it.jsModule) {
        return `loadJs(basePath + "${it.url}", "module");\n`;
      }
      return `loadJs(basePath + "${it.url}");\n`;
    })
    .join('');

  return cssMap + jsMap;
};

export const generateAssetLoaderFile = (assets: Assets): string => {
  const loadedAssets = generateDynamicLoadCalls(assets);

  return `// THIS FILE IS GENERATED; DO NOT MODIFY BY HAND.

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
${loadedAssets}}`;
};
