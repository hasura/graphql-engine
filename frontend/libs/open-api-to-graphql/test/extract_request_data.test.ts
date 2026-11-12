import { expect, test } from 'vitest';

import { getBaseUrl } from '../src/oas_3_tools';
import { extractRequestDataFromArgs } from '../src/resolver_builder';
import { ParameterObject } from '../src/types/oas3';
import { Operation } from '../src/types/operation';
import { PreprocessingData } from '../src/types/preprocessing_data';

type Args = Record<string, unknown>;

const parameters: ParameterObject[] = [
  { name: 'userId', in: 'path', required: true, schema: { type: 'string' } },
  { name: 'page', in: 'path', required: true, schema: { type: 'integer' } },
];
const data = {
  options: { simpleNames: false },
} as unknown as PreprocessingData<unknown, unknown, Args>;

test('Path parameters are substituted', () => {
  const { path } = extractRequestDataFromArgs<unknown, unknown, Args>(
    '/users/{userId}/pages/{page}',
    parameters,
    { userId: 'alice', page: 2 },
    data,
  );
  expect(path).toEqual('/users/alice/pages/2');
});

test('Path parameter values containing $ sequences are kept literally', () => {
  const { path } = extractRequestDataFromArgs<unknown, unknown, Args>(
    '/users/{userId}/pages/{page}',
    parameters,
    { userId: "a$&b$$c$'d", page: 1 },
    data,
  );
  expect(path).toEqual("/users/a$&b$$c$'d/pages/1");
});

test('Server variable defaults containing $ sequences are kept literally', () => {
  const operation = {
    servers: [
      {
        url: 'https://{host}/{base}/',
        variables: {
          host: { default: 'example.com' },
          base: { default: "v$&1$$x$'" },
        },
      },
    ],
  } as unknown as Operation;
  expect(getBaseUrl(operation)).toEqual("https://example.com/v$&1$$x$'");
});
