import nodePath from 'node:path';

interface Options {
  path: string;
}

/**
 * Split the given path using the OS-based separator.
 */
export function splitPath(options: Options) {
  return options.path.split(nodePath.sep);
}
