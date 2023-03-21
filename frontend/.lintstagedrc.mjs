// .lintstagedrc.js
export default {
  '*.{js,jsx,ts,tsx,json}': filenames =>
    'nx affected --target=lint --fix --parallel=3',
  '*': fileNames => 'nx format:write --base=main',
};
