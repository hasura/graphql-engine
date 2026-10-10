import { runNxCommandAsync, updateFile, readFile } from '@nx/plugin/testing';

export const createUtilsForLibTesting = (project: string) => {
  const generateImportLine = (scope: string, type: string, salt?: string) =>
    `import '${project}-${scope}-${type}${salt ? salt : ''}';`;
  const createStandardLib = async (
    scope: string,
    type: string,
    salt?: string,
    metaTags?: string[],
  ) => {
    // Nx only supports "as-provided" naming now, so spell out the directory and
    // project name the old "derived" format produced from
    // `--directory=<project><salt> --name=<scope>-<type>`.
    const projectDir = project + (salt ? salt : '');
    const libName = `${projectDir}-${scope}-${type}`;
    const importPath = `${project}-${scope}-${type}` + (salt ? salt : '');
    await runNxCommandAsync(
      `generate @nx/react:library --name=${libName} --importPath=${importPath} --directory=libs/${projectDir}/${scope}-${type} --tags=scope:${scope},type:${type}${
        metaTags ? ',' + metaTags.join(',') : ''
      } --no-interactive`,
    );
  };
  const runLintOnProject = async (
    scope: string,
    type: string,
  ): Promise<string> => {
    const { stdout } = await runNxCommandAsync(
      `run ${project}-${scope}-${type}:lint`,
      { silenceError: true },
    );

    const cleanedOutput = stdout.replace(
      // eslint-disable-next-line no-control-regex
      /[\u001b\u009b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g,
      '',
    );
    return cleanedOutput;
  };
  const getModuleBoundariesErrors = async (
    scope: string,
    type: string,
  ): Promise<string[]> => {
    const lintOutput = await runLintOnProject(scope, type);
    return lintOutput
      .split('\n')
      .filter((val) => val.includes('@nx/enforce-module-boundaries'))
      .map((it) => it.trim());
  };
  const getModuleBoundariesErrorsWithNames = async (
    scope: string,
    type: string,
  ): Promise<string[]> => {
    const lintOutput = await getModuleBoundariesErrors(scope, type);
    const fileContent = readFile(
      `libs/${project}/${scope}-${type}/src/lib/${project}-${scope}-${type}.tsx`,
    )
      .split('\n')
      .map((it) => it.replace(project, 'PROJECT'));

    return lintOutput.map((line) => {
      const [lineNumber, ...rest] = line.split(':');
      return [fileContent[parseInt(lineNumber) - 1], lineNumber, ...rest].join(
        ':',
      );
    });
  };

  const addLinesToFile = (scope: string, type: string, content: string) =>
    updateFile(
      `libs/${project}/${scope}-${type}/src/lib/${project}-${scope}-${type}.tsx`,
      (oldFile) => content + '\n' + oldFile,
    );

  return {
    addLinesToFile,
    runLintOnProject,
    createStandardLib,
    getModuleBoundariesErrors,
    generateImportLine,
    getModuleBoundariesErrorsWithNames,
  };
};
