import fs from 'fs';

interface Options {
  dir: string;
}

/**
 * Wrapper task around mkdirSync.
 */
export function mkdirSync(options: Options) {
  if (fs.existsSync(options.dir)) return null;

  fs.mkdirSync(options.dir);

  return null;
}
