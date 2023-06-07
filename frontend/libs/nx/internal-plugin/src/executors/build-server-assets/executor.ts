import { BuildServerAssetsExecutorSchema } from './schema';
import { ExecutorContext } from '@nrwl/devkit';
import runCommands from 'nx/src/executors/run-commands/run-commands.impl';
import { flushChanges, FsTree, printChanges } from 'nx/src/generators/tree';
import * as fs from 'node:fs';

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

function listFilesExtensions(directory: string, ignoredFiles: string[]) {
  // Read the contents of the folder
  const files = fs.readdirSync(directory);

  // Filter out ignored files
  const filteredFiles = files.filter(file => !ignoredFiles.includes(file));

  // Extract the file extensions from the list of files
  const extensions = filteredFiles.map(file => {
    const parts = file.split('.');
    return parts.length > 1 ? parts[parts.length - 1] : '';
  });

  // Remove duplicates and empty strings
  return [...new Set(extensions.filter(extension => extension !== ''))];
}

export default async function runMyExecutor(
  options: BuildServerAssetsExecutorSchema,
  context: ExecutorContext
) {
  const projectName = context.projectName;
  if (!projectName) {
    throw new Error('No project name was given.');
  }
  const distTarget =
    context.workspace?.projects?.[projectName]?.targets?.build?.options
      ?.outputPath ?? `dist/apps/${projectName}`;
  const serverAssetBase = `dist/apps/server-assets-${projectName}`;

  await runCommands(
    {
      commands: [
        `rm -rf ${serverAssetBase}`,
        `cp -r ${distTarget} ${serverAssetBase}`,
        `mkdir -p ${serverAssetBase}/versioned`,
      ],
      parallel: false,
      __unparsed__: [],
    },
    context
  );

  const listOfExtToCopy = listFilesExtensions(serverAssetBase, ['index.html']);

  const tree = new FsTree(context.root, true);

  const rawHtmlString = tree.read(distTarget + '/index.html');

  if (!rawHtmlString) {
    throw new Error('No html found.');
  }

  const htmlString = rawHtmlString.toString();

  const extractedAssets = extractAssets(htmlString);
  validateAllowedAssets(extractedAssets);

  console.log('This will be the loaded assets from this build :');
  console.log('Javascript assets :');
  extractedAssets.js.forEach(it => console.log('- ' + it.url));
  console.log('CSS assets :');
  extractedAssets.css.forEach(it => console.log('- ' + it.url));

  const finalAssets = gzAssetNames(extractedAssets);

  const loaderFile = generateAssetLoaderFile(finalAssets);
  tree.write(`${serverAssetBase}/versioned/assetLoader.js`, loaderFile);
  tree.write(
    `${serverAssetBase}/versioned/main.js`,
    generatePolyfillLoaderFile(loaderFile)
  );

  printChanges(tree.listChanges());
  flushChanges(context.root, tree.listChanges());

  await runCommands(
    {
      commands: [
        ...listOfExtToCopy.map(
          ext => `mv ${serverAssetBase}/*.${ext} ${serverAssetBase}/versioned`
        ),
        ...[
          ...extractedAssets.css.map(css => css.url),
          ...extractedAssets.js.map(js => js.url),
          'assetLoader.js',
          'main.js',
        ].map(name => `gzip -f ${serverAssetBase}/versioned/${name}`),
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
