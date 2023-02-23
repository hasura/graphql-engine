import { BuildServerAssetsExecutorSchema } from './schema';
import { ExecutorContext } from '@nrwl/devkit';
import runCommands from 'nx/src/executors/run-commands/run-commands.impl';
import { flushChanges, FsTree, printChanges } from 'nx/src/generators/tree';

import {
  extractAssets,
  generateAssetLoaderFile,
} from 'unplugin-dynamic-asset-loader';

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
  "This file cannot be used anymore give it was used prior to 2.18 and we can't use that file name anymore since cli may load this instead of the correct assets.";
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

export const gzAssetNames = (assets: Assets): Assets => {
  return {
    css: assets.css.map(asset => ({ ...asset, url: `${asset.url}.gz` })),
    js: assets.js.map(asset => ({ ...asset, url: `${asset.url}.gz` })),
  };
};

export const generatePolyfillLoaderFile = (
  assetLoaderString: string
): string => {
  return `// This file exists for older cli using newer asset names.
${assetLoaderString}

// This is from the old console template for the CLI
window.__loadConsoleAssetsFromBasePath(window.__env.versionedAssetsPath);

console.log('Please note that the error of loading vendor.js and main.css is normal.');

`;
};

export default async function runMyExecutor(
  options: BuildServerAssetsExecutorSchema,
  context: ExecutorContext
) {
  const projectName = context.projectName;

  if (!projectName || typeof projectName !== 'string') {
    console.log(`Unexpected project name ${projectName}`);
    return { success: false };
  }

  const distTarget =
    context.workspace.projects[
      projectName
    ]?.targets?.build?.options?.outputPath.toString() ??
    `dist/apps/${projectName}`;

  if (typeof distTarget !== 'string') {
    console.log(`Unexpected dist target ${distTarget}`);
    return { success: false };
  }

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

  const indexHtmlFile = tree.read(distTarget + '/index.html');

  if (!indexHtmlFile) {
    console.log(`${distTarget}/index.html file not found`);
    return { success: false };
  }

  const htmlString = indexHtmlFile.toString();

  const extractedAssets = gzAssetNames(extractAssets(htmlString));

  console.log('This will be the loaded assets from this build :');
  console.log(extractedAssets.js.map(it => it.url));
  console.log(extractedAssets.css.map(it => it.url));

  validateAllowedAssets(extractedAssets);
  const loaderFile = generateAssetLoaderFile(extractedAssets);
  tree.write(`${serverAssetBase}/versioned/assetLoader.js`, loaderFile);
  tree.write(
    `${serverAssetBase}/versioned/main.js`,
    generatePolyfillLoaderFile(loaderFile)
  );
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
        `gzip -f ${serverAssetBase}/versioned/main.js`,
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
