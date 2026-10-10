import { CODEGEN_REPO, BASE_CODEGEN_PATH } from '../../constants';
import endpoints from '../../../../Endpoints';
import { request } from '@hasura/shared/utils';

// Frameworks whose templater is bundled directly into the console instead of
// being fetched from assets/common/codegen at runtime, so they can run
// without depending on globals injected into an eval'd/`new Function`-built
// scope.
const localCodegenTemplaters: Record<
  string,
  () => Promise<{ templater: (...args: any[]) => unknown }>
> = {
  'nodejs-express': () => import('./templates/nodejs-express'),
  'nodejs-zeit': () => import('./templates/nodejs-zeit'),
  'nodejs-azure-function': () => import('./templates/nodejs-azure-function'),
  'typescript-zeit': () => import('./templates/typescript-zeit'),
};

export const getCodegenFilePath = (framework: string) => {
  return `${BASE_CODEGEN_PATH}/${framework}/actions-codegen.js`;
};

export const getStarterKitPath = (framework: string) => {
  return `https://github.com/${CODEGEN_REPO}/tree/master/${framework}/starter-kit/`;
};

export const getStarterKitDownloadPath = (framework: string) => {
  return `https://github.com/${CODEGEN_REPO}/raw/master/${framework}/${framework}.zip`;
};
export const getGlitchProjectURL = () => {
  return 'https://glitch.com/edit/?utm_content=project_hasura-actions-starter-kit&utm_source=remix_this&utm_medium=button&utm_campaign=glitchButton#!/remix/hasura-actions-starter-kit';
};

export const GLITCH_PROJECT_URL = '';

export const getCodegenFunc = (framework: string) => {
  const localTemplater = localCodegenTemplaters[framework];
  if (localTemplater) {
    return localTemplater().then((mod) => mod.templater);
  }

  // remaining frameworks under assets/common/codegen are self-contained
  // bundles fetched at runtime; `new Function` executes them and returns
  // their exported `templater`, without relying on eval's implicit lexical
  // scope capture, which minification can break.
  return request(getCodegenFilePath(framework))
    .then((response) => response.text())
    .then((rawJsString) => {
      const buildCodegenerator = new Function(
        `${rawJsString}\nreturn templater;`,
      );
      return buildCodegenerator();
    });
};

export const getFrameworkCodegen = (
  framework: string,
  actionName: string,
  actionsSdl: string,
  parentOperation: string,
) => {
  return getCodegenFunc(framework).then((codegenerator) => {
    const derive = {
      operation: parentOperation,
      endpoint: endpoints.graphQLUrl,
    };
    const codegenFiles = codegenerator(actionName, actionsSdl, derive);
    return codegenFiles;
  });
};
