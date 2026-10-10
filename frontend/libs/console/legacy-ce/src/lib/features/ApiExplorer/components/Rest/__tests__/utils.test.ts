import { getRequestBody } from '../utils';

describe('../utils.ts/getRequestBody returns', () => {
  it('undefined when no URL variables and form state vars are present', () => {
    const res = getRequestBody({
      urlQueryVariables: [],
      variableState: [],
    });
    expect(res).toMatchInlineSnapshot(`undefined`);
  });

  it('a response containing only form state vars when no URL vars are present', () => {
    const res = getRequestBody({
      urlQueryVariables: [],
      variableState: [
        {
          name: 'name',
          kind: 'NonNullType',
          type: 'String',
          value: 'some value',
        },
      ],
    });
    expect(res).toMatchInlineSnapshot(`"{"name":"some value"}"`);
  });

  it('a response containing form state vars from which URL vars have been filtered off', () => {
    const res = getRequestBody({
      urlQueryVariables: [
        {
          type: 'variable',
          value: 'id',
        },
      ],
      variableState: [
        {
          name: 'id',
          kind: 'NonNullType',
          type: 'Int',
          value: '1',
        },
        {
          name: 'name',
          kind: 'NonNullType',
          type: 'String',
          value: 'update_value',
        },
      ],
    });
    expect(res).toMatchInlineSnapshot(`"{"name":"update_value"}"`);
  });
});
