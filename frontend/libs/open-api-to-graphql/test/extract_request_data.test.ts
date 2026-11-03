import { expect, test } from 'vitest';

import { extractRequestDataFromArgs } from '../src/resolver_builder';
import { ParameterObject } from '../src/types/oas3';

const parameters: ParameterObject[] = [
  { name: 'userId', in: 'path', required: true, schema: { type: 'string' } },
  { name: 'page', in: 'path', required: true, schema: { type: 'integer' } },
];
const data = { options: { simpleNames: false } } as any;

test('Path parameters are substituted', () => {
  const { path } = extractRequestDataFromArgs(
    '/users/{userId}/pages/{page}',
    parameters,
    { userId: 'alice', page: 2 },
    data,
  );
  expect(path).toEqual('/users/alice/pages/2');
});

test('Path parameter values containing $ sequences are kept literally', () => {
  const { path } = extractRequestDataFromArgs(
    '/users/{userId}/pages/{page}',
    parameters,
    { userId: "a$&b$$c$'d", page: 1 },
    data,
  );
  expect(path).toEqual("/users/a$&b$$c$'d/pages/1");
});
