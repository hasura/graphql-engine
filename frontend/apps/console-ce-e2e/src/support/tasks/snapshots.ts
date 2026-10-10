import * as fs from 'fs';
import * as path from 'path';
import * as vm from 'vm';

// Node-side storage for the `toMatchSnapshot` command (see support/snapshots.ts).
// Snapshots live in `__snapshots__/<spec file name>.snap` next to the spec, in
// the `exports[`<title>`] = <json>;` format used by Jest and by the former
// cypress-plugin-snapshots, so existing snapshot files keep working.

type SnapshotStore = Record<string, unknown>;

type SnapshotArgs = {
  specFile: string;
  snapshotTitle: string;
};

function getSnapshotFilename(specFile: string) {
  return path.join(
    path.dirname(specFile),
    '__snapshots__',
    `${path.basename(specFile)}.snap`,
  );
}

function readStore(filename: string): SnapshotStore {
  if (!fs.existsSync(filename)) {
    return {};
  }

  const sandbox = { exports: {} as SnapshotStore };
  vm.runInNewContext(fs.readFileSync(filename, 'utf8'), sandbox, { filename });
  return sandbox.exports;
}

function escapeTemplateLiteral(value: string) {
  return value
    .replace(/\\/g, '\\\\')
    .replace(/`/g, '\\`')
    .replace(/\$\{/g, '\\${');
}

function writeStore(filename: string, store: SnapshotStore) {
  const content = Object.keys(store)
    .map(
      (key) =>
        `exports[\`${escapeTemplateLiteral(key)}\`] =\n${JSON.stringify(
          store[key],
          undefined,
          2,
        )};`,
    )
    .join('\n\n');

  fs.mkdirSync(path.dirname(filename), { recursive: true });
  fs.writeFileSync(filename, `${content}\n`);
}

/**
 * Returns the stored snapshot, or `null` when there is no snapshot with that title.
 */
export function readSnapshot({ specFile, snapshotTitle }: SnapshotArgs) {
  const snapshotFile = getSnapshotFilename(specFile);
  const store = readStore(snapshotFile);

  return {
    snapshotFile,
    expected: snapshotTitle in store ? store[snapshotTitle] : null,
    exists: snapshotTitle in store,
  };
}

export function writeSnapshot({
  specFile,
  snapshotTitle,
  value,
}: SnapshotArgs & { value: unknown }) {
  const snapshotFile = getSnapshotFilename(specFile);
  const store = readStore(snapshotFile);
  store[snapshotTitle] = value;
  writeStore(snapshotFile, store);

  return snapshotFile;
}
