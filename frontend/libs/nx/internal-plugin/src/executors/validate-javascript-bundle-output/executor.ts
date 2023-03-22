import { ValidateJavascriptBundleOutputExecutorSchema } from './schema';
import { globSync } from 'glob';
import { ExecutorContext } from '@nrwl/devkit';
import * as fs from 'fs';

type CheckerFunction = (
  fileContent: string,
  fileName: string
) =>
  | { ok: true; fileName: string }
  | { ok: false; errors: string[]; fileName: string };

const createForbiddenString =
  (patern: string): CheckerFunction =>
  (fileContent, fileName) => {
    if (fileContent.includes(patern)) {
      return {
        ok: false,
        errors: [`"${patern}" should not be found in the bundle.`],
        fileName,
      };
    }
    return { ok: true, fileName };
  };

const createForbiddenEnv = (envVariableName: string): CheckerFunction => {
  const envValue = process.env[envVariableName];
  return (fileContent, fileName) => {
    if (!envValue) {
      return { ok: true, fileName };
    }
    if (fileContent.includes(envValue)) {
      return {
        ok: false,
        errors: [
          `process.env.${envVariableName} value should not be found in the bundle.`,
        ],
        fileName,
      };
    }
    return { ok: true, fileName };
  };
};

const checks = [
  createForbiddenString('NX_CLOUD_ACCESS_TOKEN'),
  createForbiddenEnv('NX_CLOUD_ACCESS_TOKEN'),
];

export default async function runExecutor(
  options: ValidateJavascriptBundleOutputExecutorSchema,
  context: ExecutorContext
) {
  const projectName = context.projectName;
  if (!projectName) {
    throw new Error('No project name was given.');
  }
  const distTarget =
    context.workspace?.projects?.[projectName]?.targets?.build?.options
      ?.outputPath ?? `dist/apps/${projectName}`;

  const allFilesWeShouldLookAt = globSync('**/*.{js,css,map,txt,json}', {
    cwd: distTarget,
  });

  const results = allFilesWeShouldLookAt
    .map(it => ({
      fileName: it,
      fileContent: fs.readFileSync(distTarget + '/' + it, 'utf8'),
    }))
    .map(({ fileName, fileContent }) =>
      checks.map(check => check(fileContent, fileName))
    )
    .flat();

  const errorMessages = results
    .map(it => {
      if (it.ok) {
        return '';
      }
      return it.errors.map(err => `${it.fileName}: ${err}`);
    })
    .flat()
    .filter(value => value !== '');

  if (errorMessages.length > 0) {
    console.error(
      'Found ' + errorMessages.length + ' critical issues, aborting.'
    );
    errorMessages.forEach(error => console.error('- ' + error));
    throw new Error('Non valid bundle.');
  }
  console.log('No issues found in the bundle.');

  return {
    success: true,
  };
}
