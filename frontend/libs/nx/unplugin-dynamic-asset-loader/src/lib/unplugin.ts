import { createUnplugin } from 'unplugin';
import * as webpack from 'webpack';
import { generateAssetLoaderFile, extractAssets } from './assetLoader';

const { RawSource } = webpack.sources;
const { Compilation } = webpack;

const pluginName = 'unplugin-prefixed-name';

const getFileFromHtml = (html: string): string => {
  let assets = extractAssets(html);
  assets = {
    ...assets,
    // Filter out webpack styles js file
    js: assets.js.filter(asset => asset.url !== 'styles.js'),
  };
  return generateAssetLoaderFile(assets);
};

const unplugin = createUnplugin(label => ({
  name: pluginName,
  // webpack's id filter is outside of loader logic,
  // an additional hook is needed for better perf on webpack
  transformInclude(id) {
    return id.endsWith('index.html');
  },
  buildStart() {
    // this.addWatchFile('index.html');
  },
  // just like rollup transform
  transform(code, id) {
    // this.warn('HEEEE' + code);
    // console.log(code);
    return code;
  },

  watchChange(id) {
    // console.log('WATCH CHANGES ', id);
    // const content = this.parse(id);
    // console.log(content);
  },
  webpack(compiler) {
    // Since the html is comming from a plugin and not in the normal flow of assets
    // We need to tap webpack directly
    compiler.hooks.thisCompilation.tap(pluginName, compilation => {
      compilation.hooks.processAssets.tap(
        {
          name: pluginName,

          // Using one of the later asset processing stages to ensure
          // that all assets were already added to the compilation by other plugins.
          stage: Compilation.PROCESS_ASSETS_STAGE_SUMMARIZE,
        },
        assets => {
          // console.log('HOOK START');

          const indexHtmlSource = compilation
            .getAsset('index.html')
            ?.source?.source();

          const finalString = getFileFromHtml(
            (indexHtmlSource as string) ?? ''
          );

          // console.log(indexHtmlSource);

          console.log('CREATING ASSET LOADER : ', finalString);
          compilation.emitAsset('assetLoader.js', new RawSource(finalString));
          // console.log(Object.keys(assets));
          // console.log('HOOK END');
        }
      );
    });
  },
}));

export const vitePlugin = unplugin.vite;
export const rollupPlugin = unplugin.rollup;
export const webpackPlugin = unplugin.webpack;
export const esbuildPlugin = unplugin.esbuild;
