import {
  CheckerFunction,
  createForbiddenEnv,
  createForbiddenFileName,
  createForbiddenString,
} from './validators';

type ParamsOfChecker = Parameters<CheckerFunction>[0];

type ArgsWithOnlyFileNameRequired = Partial<ParamsOfChecker>;

type ValidatorRun = {
  validatorFactory: () => CheckerFunction;
  valid: {
    title: string;
    arg: ArgsWithOnlyFileNameRequired;
    setup?: () => void;
  }[];
  invalid: {
    title: string;
    arg: ArgsWithOnlyFileNameRequired;
    setup?: () => void;
    error: string;
  }[];
};

const runValidatorCheck = ({
  valid,
  validatorFactory,
  invalid,
}: ValidatorRun) => {
  const defaultArgs: ParamsOfChecker = {
    fileName: 'my.js',
    distTarget: 'something',
    fileContent: '',
  };

  for (const validCase of valid) {
    it(validCase.title, () => {
      validCase.setup?.();

      const result = validatorFactory()({ ...defaultArgs, ...validCase.arg });

      expect(result.ok).toBeTruthy();
    });
  }
  for (const invalidCase of invalid) {
    it(invalidCase.title, () => {
      invalidCase.setup?.();

      const result = validatorFactory()({ ...defaultArgs, ...invalidCase.arg });

      expect(result.ok).toBeFalsy();
      if (result.ok) {
        fail();
      }
      expect(result.errors).toHaveLength(1);
      expect(result.errors[0]).toEqual(invalidCase.error);
    });
  }
};

describe('validators', () => {
  describe('createForbiddenEnv', () => {
    const setupEnvValue = () => {
      process.env['MY_FAKE_ENV'] = 'something';
    };

    runValidatorCheck({
      validatorFactory: () => createForbiddenEnv('MY_FAKE_ENV'),
      valid: [
        {
          title: 'should ignore other values',
          arg: {
            fileContent: 'my super value',
          },
          setup: setupEnvValue,
        },
      ],
      invalid: [
        {
          title: 'should catch a env that should not be there',
          arg: {
            fileContent: 'there is something wierd',
          },
          error:
            'process.env.MY_FAKE_ENV value should not be found in the bundle.',
          setup: setupEnvValue,
        },
        {
          title: 'should catch a value even if there is multiple lines',
          arg: {
            fileContent: `Hello
world
something`,
          },
          error:
            'process.env.MY_FAKE_ENV value should not be found in the bundle.',
          setup: setupEnvValue,
        },
      ],
    });
  });

  describe('createForbiddenString', () => {
    runValidatorCheck({
      validatorFactory: () => createForbiddenString('something'),
      valid: [
        {
          title: 'should ignore other values',
          arg: {
            fileContent: 'my super value',
          },
        },
      ],
      invalid: [
        {
          title: 'should catch a env that should not be there',
          arg: {
            fileContent: 'there is something wierd',
          },
          error: '"something" should not be found in the bundle.',
        },
        {
          title: 'should catch a value even if there is multiple lines',
          arg: {
            fileContent: `Hello
world
something`,
          },
          error: '"something" should not be found in the bundle.',
        },
      ],
    });
  });

  describe('createForbiddenFileName', () => {
    describe('with a string as option', () => {
      runValidatorCheck({
        validatorFactory: () =>
          createForbiddenFileName({ forbiddenName: 'main.js' }),
        valid: [
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'toto.js',
            },
          },
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'toto.css',
            },
          },
        ],
        invalid: [
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'main.js',
            },
            error: 'main.js is matching "main.js" forbidden file name.',
          },
        ],
      });
    });

    describe('with a regex as option', () => {
      runValidatorCheck({
        validatorFactory: () =>
          createForbiddenFileName({ forbiddenName: /vendor\..*\.js\.map/ }),
        valid: [
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'toto.js',
            },
          },
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'vendor.js',
            },
          },
        ],
        invalid: [
          {
            title: 'should ignore other values',
            arg: {
              fileName: 'vendor.something.js.map',
            },
            error:
              'vendor.something.js.map is matching "/vendor\\..*\\.js\\.map/" forbidden file name.',
          },
        ],
      });
    });
  });
});
