import { Parser } from 'htmlparser2';

export type Asset = {
  tag: string;
  url: string;
};

// Escape a (possibly entity-decoded) attribute value so the serialised tag is
// valid HTML and a value containing `"` or `&` can't corrupt the markup. Only the
// serialised `tag` string is escaped — the `url`/`href` read off `attribs` is kept
// verbatim (it is used as an on-disk asset path, not re-parsed as HTML).
const escapeAttrValue = (value: string): string =>
  value.replace(/&/g, '&amp;').replace(/"/g, '&quot;');

/** Serialise an opening tag from htmlparser2 attributes, preserving order. */
const serializeTag = (
  name: string,
  attribs: Record<string, string>,
  selfClosing: boolean,
): string => {
  const attrs = Object.entries(attribs)
    .map(([key, value]) =>
      value === '' ? key : `${key}="${escapeAttrValue(value)}"`,
    )
    .join(' ');
  const open = `<${name}${attrs ? ` ${attrs}` : ''}`;
  return selfClosing ? `${open}/>` : `${open}></${name}>`;
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

  // Use htmlparser2 (a real streaming HTML parser) rather than a naive regex
  // parser: the webpack-generated index.html contains a large inline env-var
  // <script>, multiple hashed CSS/JS chunks and `type="module"` script tags, which
  // the previous `dom-parser` choked on (`Cannot read properties of null (reading
  // 'isSelfCloseTag')`). htmlparser2 treats <script>/<style> bodies as raw text,
  // so inline JS/CSS can't be mis-parsed as markup.
  const parser = new Parser(
    {
      onopentag(name, attribs) {
        if (name === 'script') {
          const src = attribs.src;
          if (src && !src.startsWith('http')) {
            assets.js.push({
              tag: serializeTag('script', attribs, false),
              url: src,
              jsModule: attribs.type === 'module',
              type: 'js',
            });
          }
        } else if (name === 'link') {
          const href = attribs.href;
          if (
            attribs.rel === 'stylesheet' &&
            href &&
            !href.startsWith('http')
          ) {
            assets.css.push({
              tag: serializeTag('link', attribs, true),
              url: href,
              type: 'css',
            });
          }
        }
      },
    },
    // Recognise `<script .../>` self-closing shorthand (used in the unit tests and
    // tolerated by some tooling) so following siblings aren't swallowed as script body.
    { recognizeSelfClosing: true },
  );
  parser.write(html);
  parser.end();

  if (assets.css.length === 0) {
    throw new Error(
      'No css assets found, there is an issue with the provided html.',
    );
  }
  if (assets.js.length === 0) {
    throw new Error(
      'No js assets found, there is an issue with the provided html.',
    );
  }
  return assets;
};

export const generateDynamicLoadCalls = (assets: Assets): string => {
  const cssMap = assets.css
    .map((it) => `loadCss(basePath + "${it.url}");\n`)
    .join('');

  const jsMap = assets.js
    .map((it) => {
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
