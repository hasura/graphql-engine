export type CheckerFunction = (args: {
  fileContent: string;
  fileName: string;
  distTarget: string;
}) => { ok: true } | { ok: false; errors: string[] };

export const createForbiddenString =
  (pattern: string): CheckerFunction =>
  ({ fileContent, fileName }) => {
    if (fileContent.includes(pattern)) {
      return {
        ok: false,
        errors: [`"${pattern}" should not be found in the bundle.`],
      };
    }
    return { ok: true };
  };

export const createForbiddenEnv = (
  envVariableName: string
): CheckerFunction => {
  const envValue = process.env[envVariableName];
  return ({ fileContent, fileName }) => {
    if (!envValue) {
      return { ok: true, fileName };
    }
    if (fileContent.includes(envValue)) {
      return {
        ok: false,
        errors: [
          `process.env.${envVariableName} value should not be found in the bundle.`,
        ],
      };
    }
    return { ok: true };
  };
};

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
  return ({ fileName }) => {
    if (fileChecker(fileName)) {
      return {
        ok: false,
        errors: [
          `${fileName} is matching "${forbiddenName}" forbidden file name.${
            extraReason ? ' ' + extraReason : ''
          }`,
        ],
      };
    }
    return { ok: true };
  };
};
