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

export const createForbiddenFileName = ({
  forbiddenName,
  extraReason,
}: {
  forbiddenName: string | RegExp;
  extraReason?: string;
}): CheckerFunction => {
  const fileChecker = fileName => {
    if (typeof forbiddenName === 'string') {
      return fileName === forbiddenName;
    }
    return forbiddenName.test(fileName);
  };
  return (contnet, fileName) => {
    if (fileChecker(fileName)) {
      return {
        ok: false,
        errors: [
          `${fileName} is matching "${forbiddenName}" forbidden file name.${
            extraReason ? ' ' + extraReason : ''
          }`,
        ],
        fileName,
      };
    }
    return { ok: true, fileName };
  };
};

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

const legacyNamesThatWeCantUseReason =
  'This file name cannot be used anymore given it was used prior to 2.18 and cli will load this instead of the correct assets.';

const checks = [
  createForbiddenString('NX_CLOUD_ACCESS_TOKEN'),
  createForbiddenEnv('NX_CLOUD_ACCESS_TOKEN'),
  createForbiddenFileName({
    forbiddenName: /vendor\..*\.js\.map/,
    extraReason:
      "Due to limitation on sentry, we don't want to ship vendor source maps.",
  }),
  createForbiddenFileName({
    forbiddenName: 'main.css',
    extraReason: legacyNamesThatWeCantUseReason,
  }),
  createForbiddenFileName({
    forbiddenName: 'main.js',
    extraReason: legacyNamesThatWeCantUseReason,
  }),
  createForbiddenFileName({
    forbiddenName: 'assetLoader.js',
  }),
  createForbiddenFileName({
    forbiddenName: 'vendor.js',
    extraReason: legacyNamesThatWeCantUseReason,
  }),
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
