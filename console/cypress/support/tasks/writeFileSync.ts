import fs from 'fs';

interface Options {
  file: string;
  data: string;
}

/**
 * Wrapper task around writeFileSync.
 */
export function writeFileSync(options: Options) {
  fs.writeFileSync(options.file, options.data);

  return null;
}
