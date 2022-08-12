import nodePath from 'node:path';

interface Options {
  path: string[];
}

/**
 * Join the given path using the OS-based separator.
 */
export function joinPath(options: Options) {
  return options.path.join(nodePath.sep);
}
