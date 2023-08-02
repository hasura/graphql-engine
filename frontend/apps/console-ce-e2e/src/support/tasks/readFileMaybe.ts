import * as fs from 'fs';

// https://github.com/cypress-io/snapshot#userelativesnapshots
export function readFileMaybe(filename: string) {
  if (fs.existsSync(filename)) {
    return fs.readFileSync(filename, 'utf8');
  }

  return '{}';
}
