import { BuildServerAssetsExecutorSchema } from './schema';
import { ExecutorContext } from '@nrwl/devkit';
import runCommands from 'nx/src/executors/run-commands/run-commands.impl';
import { flushChanges, FsTree, printChanges } from 'nx/src/generators/tree';
import * as DomParser from 'dom-parser';

type Asset = {
  tag: string;
  url: string;
};

type JsAsset = Asset & {
  type: 'js';
  jsModule?: boolean;
};

type CssAsset = Asset & {
  type: 'css';
};

type Assets = {
  js: JsAsset[];
  css: CssAsset[];
};

const legacyNamesThatWeCantUseReason =
  "This file cannot be used anymore give it was used prior to 2.18 and we can't use that file name anymore since older cli may load this instead of the correct assets.";
const forbiddenAssets: Record<string, string> = {
  'main.css': legacyNamesThatWeCantUseReason,
  'main.js': legacyNamesThatWeCantUseReason,
  'vendor.js': legacyNamesThatWeCantUseReason,
};

export const validateAllowedAssets = (assets: Assets): void => {
  for (const cssEntry of assets.css) {
    if (forbiddenAssets[cssEntry.url]) {
      throw new Error(
        `Cannot use ${cssEntry.url}: ${forbiddenAssets[cssEntry.url]}`
      );
    }
  }
  for (const jsEntry of assets.js) {
    if (forbiddenAssets[jsEntry.url]) {
      throw new Error(
        `Cannot use ${jsEntry.url}: ${forbiddenAssets[jsEntry.url]}`
      );
    }
  }
};

export const extractAssets = (html: string): Assets => {
  const assets: Assets = { js: [], css: [] };
  const domParser = new DomParser();
  const parsedDocument = domParser.parseFromString(html);
  parsedDocument.getElementsByTagName('script')?.forEach(element => {
    if (element.getAttribute('src')) {
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
      if (element.getAttribute('href')) {
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
    .map(it => `loadCss(basePath + "${it.url}.gz");\n`)
    .join('');

  const jsMap = assets.js
    .map(it => {
      if (it.jsModule) {
        return `loadJs(basePath + "${it.url}.gz", { type: "module" });\n`;
      }
      return `loadJs(basePath + "${it.url}.gz");\n`;
    })
    .join('');

  return cssMap + jsMap;
};

export const generateAssetLoaderFile = (assets: Assets): string => {
  const loadedAssets = generateDynamicLoadCalls(assets);

  console.log('This will be the loaded assets from this build :');
  console.log(loadedAssets);

  return `// THIS FILE IS GENERATED; DO NOT MODIFY BY HAND.

const loadCss = (url) => {
  const linkElem = document.createElement("link");
  linkElem.rel = "stylesheet";
  linkElem.charset = "UTF-8";
  linkElem.href = url;
  document.body.append(linkElem);
};
const loadJs = (url, { type }) => {
  const scriptElem = document.createElement("script");
  scriptElem.charset = "UTF-8";
  scriptElem.src = url;
  if (type) {
    scriptElem.type = type
  }
  document.body.append(scriptElem);
};

window.__loadConsoleAssetsFromBasePath = (basePath) => {
${loadedAssets}}`;
};

export const generatePolyfillLoaderFile = (
  assetLoaderString: string
): string => {
  return `// This file exists for older cli using newer asset names.
${assetLoaderString}

// This is from the old console template for the CLI
window.__loadConsoleAssetsFromBasePath(window.__env.versionedAssetsPath);

console.log('Please update your CLI to the latest version to remove the console network errors.');

`;
};

export default async function runMyExecutor(
  options: BuildServerAssetsExecutorSchema,
  context: ExecutorContext
) {
  const projectName = context.projectName;
  const distTarget =
    context.workspace.projects[projectName]?.targets?.build?.options
      ?.outputPath ?? `dist/apps/${projectName}`;
  const serverAssetBase = `dist/apps/server-assets-${projectName}`;
  await runCommands(
    {
      commands: [
        `rm -rf ${serverAssetBase}`,
        `cp -r ${distTarget} ${serverAssetBase}`,
        `mkdir -p ${serverAssetBase}/versioned`,
        `mv ${serverAssetBase}/*.css ${serverAssetBase}/versioned`,
        `mv ${serverAssetBase}/*.js ${serverAssetBase}/versioned`,
        `gzip -r -f ${serverAssetBase}/versioned`,
        `mv ${serverAssetBase}/*.map  ${serverAssetBase}/*.txt ${serverAssetBase}/versioned`,
      ],
      parallel: false,
      __unparsed__: [],
    },
    context
  );
  const tree = new FsTree(context.root, true);

  const htmlString = tree.read(distTarget + '/index.html').toString();

  const extractedAssets = extractAssets(htmlString);
  validateAllowedAssets(extractedAssets);
  const loaderFile = generateAssetLoaderFile(extractedAssets);
  tree.write(`${serverAssetBase}/versioned/assetLoader.js`, loaderFile);
  tree.write(
    `${serverAssetBase}/loaderPolyfill.js`,
    generatePolyfillLoaderFile(loaderFile)
  );

  printChanges(tree.listChanges());
  flushChanges(context.root, tree.listChanges());
  await runCommands(
    {
      commands: [
        `gzip -f ${serverAssetBase}/versioned/assetLoader.js`,
        `gzip -f ${serverAssetBase}/loaderPolyfill.js`,
      ],
      parallel: false,
      __unparsed__: [],
    },
    context
  );
  return {
    success: true,
  };
}
