import {
  checkFilesExist,
  readFile,
  runNxCommandAsync,
  uniq,
  tmpProjPath,
  cleanup,
  patchPackageJsonForPlugin,
} from '@nrwl/nx-plugin/testing';
import { createUtilsForLibTesting } from './utils';
import { ensureDirSync } from 'fs-extra';
import { dirname, join } from 'path';
import { writeFileSync } from 'fs';
import { execSync } from 'child_process';
import { getPackageManagerCommand } from 'nx/src/utils/package-manager';

function runNxNewCommand(args?: string, silent = false) {
  const localTmpDir = dirname(tmpProjPath());
  execSync(
    `node ${require.resolve(
      'nx'
    )} new proj --nx-workspace-root=${localTmpDir} --no-interactive --skip-install --collection=@nx/workspace --npmScope=proj --preset=empty ${
      args || ''
    }`,
    {
      cwd: localTmpDir,
      stdio: 'inherit',
    }
  );
  writeFileSync(join(tmpProjPath(), 'yarn.lock'), '');
}

function runPackageManagerInstall() {
  const pmc = getPackageManagerCommand();
  console.log('PACKAGE MANAGER RAN : ', pmc.install);
  const install = execSync(pmc.install, {
    cwd: tmpProjPath(),
    stdio: 'inherit',
  });
  return install ? install.toString() : '';
}

describe('internal-generators e2e', () => {
  // Setting up individual workspaces per
  // test can cause e2e runs to take a long time.
  // For this reason, we recommend each suite only
  // consumes 1 workspace. The tests should each operate
  // on a unique project in the workspace, such that they
  // are not dependant on one another.
  beforeAll(async () => {
    // We can't use the official utility directly since it doesn't create a yarn.lock
    // and yarn complain about being in a nested workspace
    // Workaround: create an empty yarn.lock file before install

    // ensureNxProject('@hasura/internal-plugin', 'dist/libs/nx/internal-plugin');

    ensureDirSync(tmpProjPath());
    cleanup();
    runNxNewCommand('', true);
    patchPackageJsonForPlugin(
      '@hasura/internal-plugin',
      'dist/libs/nx/internal-plugin'
    );
    runPackageManagerInstall();
    await runNxCommandAsync(
      `generate @nrwl/react:library --name=seed-lib --no-interactive`
    );
  }, 320_000);

  afterAll(() => {
    // `nx reset` kills the daemon, and performs
    // some work which can help clean up e2e leftovers
    runNxCommandAsync('reset');
  });

  describe('update-generators-scope-and-types', () => {
    it('should modify the lint files in a correct manner', async () => {
      const { stdout, stderr } = await runNxCommandAsync(
        `generate @hasura/internal-plugin:update-generators-scope-and-type`,
        { silenceError: true }
      );

      console.log(stdout);
      console.error(stderr);

      expect(() => checkFilesExist(`docs/tags.md`)).not.toThrow();
      const docs = readFile(`docs/tags.md`);
      expect(docs).toEqual(expect.stringContaining('scope'));
      expect(docs).toEqual(expect.stringContaining('console'));
      expect(docs).toEqual(expect.stringContaining('shared'));
      expect(docs).toEqual(expect.stringContaining('type'));
      expect(docs).toEqual(expect.stringContaining('ui'));
      expect(docs).toEqual(expect.stringContaining('utils'));
      expect(docs).toEqual(expect.stringContaining('data'));
      expect(docs).toEqual(expect.stringContaining('feature'));
    }, 320000);

    describe('import checker', () => {
      const scopeList = ['console', 'nx-plugins', 'shared'] as const;
      const typesList = [
        'utils',
        'ui',
        'data',
        'feature',
        'app',
        'storybook',
        'e2e',
      ] as const;

      describe('scope imports', () => {
        for (const scope of scopeList) {
          const type = 'utils';

          const scopeMapData: Record<
            (typeof scopeList)[number],
            { expectedError: (typeof scopeList)[number][] }
          > = {
            console: { expectedError: ['nx-plugins'] },
            'nx-plugins': { expectedError: ['console'] },
            shared: { expectedError: ['nx-plugins', 'console'] },
          };

          it(`should validate the scope imports from ${scope}`, async () => {
            console.log('Testing ' + scope + ':' + type + '...');
            const project = uniq('import-check-scope');
            const {
              addLinesToFile,
              getModuleBoundariesErrorsWithNames,
              createStandardLib,
              generateImportLine,
            } = createUtilsForLibTesting(project);
            await runNxCommandAsync(
              `generate @hasura/internal-plugin:update-generators-scope-and-type`
            );

            await createStandardLib(scope, type);
            const linesToAdd = [];
            for (const subScope of scopeList) {
              if (subScope === scope) {
                await createStandardLib(subScope, type, '-copy');
                linesToAdd.push(generateImportLine(subScope, type, '-copy'));
                continue;
              }
              await createStandardLib(subScope, type);
              linesToAdd.push(generateImportLine(subScope, type));
            }

            addLinesToFile(scope, type, linesToAdd.join('\n'));

            const resultOfLint = await getModuleBoundariesErrorsWithNames(
              scope,
              type
            );
            expect(resultOfLint).toMatchSnapshot(
              `Import from ${scope}:${type}`
            );
            expect(resultOfLint.length).toEqual(
              scopeMapData[scope].expectedError.length
            );

            const importErrors = resultOfLint.map(it => it.split(':')[0]);

            for (const expectedError of scopeMapData[scope].expectedError) {
              expect(
                importErrors.filter(it => it.includes(expectedError))
              ).toHaveLength(1);
            }
          }, 320000);
        }
      });

      describe('type imports', () => {
        for (const type of typesList) {
          const scope = 'shared';

          const typeMapData: Record<
            (typeof typesList)[number],
            { expectedError: (typeof typesList)[number][] }
          > = {
            data: {
              expectedError: ['e2e', 'storybook', 'app', 'feature', 'ui'],
            },
            utils: {
              expectedError: [
                'data',
                'feature',
                'e2e',
                'storybook',
                'ui',
                'app',
              ],
            },
            ui: {
              expectedError: ['e2e', 'storybook', 'app', 'feature', 'data'],
            },
            app: { expectedError: ['storybook', 'e2e', 'app'] },
            storybook: { expectedError: ['app', 'e2e'] },
            e2e: { expectedError: [] },
            feature: { expectedError: ['app', 'storybook', 'e2e'] },
          };
          it(`should validate the type imports from ${type}`, async () => {
            console.log('Testing ' + scope + ':' + type + '...');
            const project = uniq('import-check-type');
            const {
              addLinesToFile,
              getModuleBoundariesErrorsWithNames,
              createStandardLib,
              generateImportLine,
            } = createUtilsForLibTesting(project);
            await runNxCommandAsync(
              `generate @hasura/internal-plugin:update-generators-scope-and-type`
            );

            await createStandardLib(scope, type);
            const linesToAdd = [];
            for (const subType of typesList) {
              if (subType === type) {
                await createStandardLib(scope, subType, '-copy');
                linesToAdd.push(generateImportLine(scope, subType, '-copy'));
                continue;
              }
              await createStandardLib(scope, subType);
              linesToAdd.push(generateImportLine(scope, subType));
            }
            addLinesToFile(scope, type, linesToAdd.join('\n'));

            const resultOfLint = await getModuleBoundariesErrorsWithNames(
              scope,
              type
            );
            expect(resultOfLint).toMatchSnapshot(
              `Import from ${scope}:${type}`
            );
            expect(resultOfLint.length).toEqual(
              typeMapData[type].expectedError.length
            );

            const importErrors = resultOfLint.map(it => it.split(':')[0]);

            for (const expectedError of typeMapData[type].expectedError) {
              expect(
                importErrors.filter(it => it.includes(expectedError))
              ).toHaveLength(1);
            }
          }, 320000);
        }
      });
    });
  });
});
