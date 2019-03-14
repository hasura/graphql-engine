import babel from 'rollup-plugin-babel';
import resolve from 'rollup-plugin-node-resolve';
import multiEntry from 'rollup-plugin-multi-entry';
import filesize from 'rollup-plugin-filesize';
import commonjs from 'rollup-plugin-commonjs';
import progress from 'rollup-plugin-progress';

let pluginOptions = [
  multiEntry(),
  resolve({
    module: true,
    jsnext: true,
    browser: true
  }),
  commonjs(),
  progress(),
  babel({
    exclude: 'node_modules/**',
  }),
  filesize({
    showGzippedSize: false,
  })
];

export default [{
  input: './src/index.js',
  output: {
    name: 'main',   // for external calls (need exports)
    file: 'bundle/js/index.js',
    format: 'umd',
  },
  moduleName: 'graphql2chartjs',
  plugins: pluginOptions,
}];
