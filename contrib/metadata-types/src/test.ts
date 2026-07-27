import test from 'ava';
import { load } from 'js-yaml';
import { default as fs } from 'fs';
import { default as path } from 'path';
import { glob } from 'glob';

interface TestConfig {
  typeDefinitionFile: string;
  jsonInputTests: Array<{
    files: string | string[];
    expectType: string;
  }>;
}

const configFilePath = path.resolve('test-config.yaml');

async function collectTestInputs() {
  const configText = await fs.promises.readFile(configFilePath, 'utf-8');
  const tests = load(configText) as TestConfig[];

  let results = [];
  for (let entry of tests) {
    console.log('Running test for file', entry.typeDefinitionFile);
    const tsFilePath = path.resolve(entry.typeDefinitionFile);

    for (let { files, expectType } of entry.jsonInputTests) {
      for (let file of await glob(files)) {
        console.log('Checking input data from', file);

        const filePath = path.resolve(file);
        const data = await fs.promises.readFile(filePath, 'utf-8');

        results.push({ file, data, tsFilePath, expectType });
      }
    }
  }

  return results;
}

async function main() {
  for (let entry of await collectTestInputs()) {
    const testFileInfo = `[TYPE]: ${entry.tsFilePath} \n[INPUT]: ${entry.file}`;

    test('Expect Pass & Get Valid Result \n' + testFileInfo, (t) => {
      t.notThrows(async () => {
        const { Convert } = await import(entry.tsFilePath);
        Convert['to' + entry.expectType](entry.data);
      }, 'Converion from data to generated type failed');
    });
  }
}

main().catch((err) => console.log('ERR IN TESTS', err));
