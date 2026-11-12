import { Oas3, ParameterObject } from '@hasura/open-api-to-graphql';
import petStore from './fixtures/petstore.json';
// test case from this issue https://github.com/hasura/graphql-engine/issues/9734
import optimizer from './fixtures/optimizer.json';
import { generateAction, generateQueryParams, parseOas } from './utils';

const tags: ParameterObject = {
  name: 'tags',
  in: 'query',
  description: 'Tags to filter by',
  required: false,
  explode: true,
  schema: {
    type: 'array',
    items: {
      type: 'string',
    },
  },
};

const status: ParameterObject = {
  name: 'status',
  in: 'query',
  description: 'Status values that need to be considered for filter',
  required: false,
  explode: true,
  schema: {
    type: 'string',
    default: 'available',
    enum: ['available', 'pending', 'sold'],
  },
};

describe('getOperations', () => {
  it('should return an array of operations', async () => {
    const parsedOas = await parseOas(petStore as unknown as Oas3);
    const operations = Object.values(parsedOas.data.operations);
    expect(operations).toHaveLength(4);
    expect(operations[0].path).toBe('/pets');
    expect(operations[0].method).toBe('get');
    expect(operations[1].path).toBe('/pets');
    expect(operations[1].method).toBe('post');
    expect(operations[2].path).toBe('/pets/{id}');
    expect(operations[2].method).toBe('get');
    expect(operations[3].path).toBe('/pets/{id}');
    expect(operations[3].method).toBe('delete');
  });

  it('throws an error if the OAS is invalid', async () => {
    await expect(parseOas({} as unknown as Oas3)).rejects.toThrow();
  });
});

describe('generateAction', () => {
  it('should return an action', async () => {
    const action = await generateAction(
      petStore as unknown as Oas3,
      'findPets',
    );
    expect(action).toEqual({
      operationId: 'findPets',
      actionType: 'query',
      // `action`/`types` are produced via `formatGraphQL` (graphql's own
      // `print(parse(sdl))`), which never appends a trailing newline after
      // the final definition and preserves the definition order that
      // `printSchema` produced. These fixtures previously expected a
      // trailing `\n` and a `scalar BigInt` / `type Pet` order that
      // `graphql@16`'s printer does not produce.
      action: 'type Query {\n  findPets(limit: Int, tags: [String]): [Pet]\n}',
      types:
        'type Pet {\n  id: BigInt!\n  name: String!\n  tag: String\n}\n\nscalar BigInt',
      description:
        'Returns all pets from the system that the user has access to\n' +
        'Nam sed condimentum est. Maecenas tempor sagittis sapien, nec rhoncus sem sagittis sit amet. Aenean at gravida augue, ac iaculis sem. Curabitur odio lorem, ornare eget elementum nec, cursus id lectus. Duis mi turpis, pulvinar ac eros ac, tincidunt varius justo. In hac habitasse platea dictumst. Integer at adipiscing ante, a sagittis ligula. Aenean pharetra tempor ante molestie imperdiet. Vivamus id aliquam diam. Cras quis velit non tortor eleifend sagittis. Praesent at enim pharetra urna volutpat venenatis eget eget mauris. In eleifend fermentum facilisis. Praesent enim enim, gravida ac sodales sed, placerat id erat. Suspendisse lacus dolor, consectetur non augue vel, vehicula interdum libero. Morbi euismod sagittis libero sed lacinia.\n' +
        '\n' +
        'Sed tempus felis lobortis leo pulvinar rutrum. Nam mattis velit nisl, eu condimentum ligula luctus nec. Phasellus semper velit eget aliquet faucibus. In a mattis elit. Phasellus vel urna viverra, condimentum lorem id, rhoncus nibh. Ut pellentesque posuere elementum. Sed a varius odio. Morbi rhoncus ligula libero, vel eleifend nunc tristique vitae. Fusce et sem dui. Aenean nec scelerisque tortor. Fusce malesuada accumsan magna vel tempus. Quisque mollis felis eu dolor tristique, sit amet auctor felis gravida. Sed libero lorem, molestie sed nisl in, accumsan tempor nisi. Fusce sollicitudin massa ut lacinia mattis. Sed vel eleifend lorem. Pellentesque vitae felis pretium, pulvinar elit eu, euismod sapien.\n',
      method: 'GET',
      baseUrl: 'http://petstore.swagger.io/api',
      path: '/pets',
      requestTransforms: undefined,
      responseTransforms: '',
      sampleInput: JSON.stringify(
        {
          action: {
            name: 'findPets',
          },
          input: {
            limit: 10,
            tags: ['foo', 'bar'],
          },
        },
        null,
        2,
      ),
      headers: [],
      queryParams:
        '{{ concat ([{{ if empty($body.input?.tags) }} [] {{ else }} concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }}) {{ end }}, "limit={{$body.input?.limit}}&"]) }}',
    });
  });

  it('throws an error if the OAS is invalid', async () => {
    await expect(
      generateAction({} as unknown as Oas3, 'findPets'),
    ).rejects.toThrow();
  });
});

describe('generateQueryParams', () => {
  it('should generate query params with one non-array param', async () => {
    const queryParams = await generateQueryParams([status]);
    expect(queryParams).toStrictEqual([
      { name: 'status', value: '{{$body.input?.status}}' },
    ]);
  });
  it('should generate query params with one non-array param and one array param', async () => {
    const queryParams = await generateQueryParams([status, tags]);
    expect(queryParams).toBe(
      '{{ concat (["status={{$body.input?.status}}&", {{ if empty($body.input?.tags) }} [] {{ else }} concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }}) {{ end }}]) }}',
    );
  });
  it('should generate query params with one non-array param and one array param (reversed)', async () => {
    const queryParams = await generateQueryParams([tags, status]);
    console.log(queryParams);
    expect(queryParams).toBe(
      '{{ concat ([{{ if empty($body.input?.tags) }} [] {{ else }} concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }}) {{ end }}, "status={{$body.input?.status}}&"]) }}',
    );
  });
});

describe('optimizer API', () => {
  it('should generate correctly', async () => {
    const action = await generateAction(
      optimizer as unknown as Oas3,
      'optimise_optimise_post',
    );
    expect(action).toMatchSnapshot();
  });
});

describe('array transforms', () => {
  const event = {
    type: 'object',
    properties: { at_time: { type: 'string' } },
  };
  const lineItem = {
    type: 'object',
    properties: {
      item_sku: { type: 'string' },
      unit_count: { type: 'integer' },
      // names that aren't valid Kriti identifiers or are Kriti keywords
      end: { type: 'array', items: event },
      '2fa_codes': { type: 'array', items: event },
      item: { type: 'array', items: event },
      // array of arrays
      event_grid: { type: 'array', items: { type: 'array', items: event } },
    },
  };
  const arrayOas = {
    openapi: '3.0.0',
    info: { title: 'orders', version: '1.0.0' },
    servers: [{ url: 'http://example.com' }],
    paths: {
      '/orders': {
        post: {
          operationId: 'createOrders',
          requestBody: {
            required: true,
            content: {
              'application/json': {
                schema: {
                  type: 'object',
                  properties: {
                    'line-items': { type: 'array', items: lineItem },
                  },
                },
              },
            },
          },
          responses: {
            '200': {
              description: 'ok',
              content: {
                'application/json': {
                  schema: { type: 'array', items: lineItem },
                },
              },
            },
          },
        },
      },
    },
  } as unknown as Oas3;

  it('ranges request arrays over a valid identifier, not the raw field name', async () => {
    const action = await generateAction(arrayOas, 'createOrders');
    const transform = action.requestTransforms?.value as string;

    expect(transform).toMatch(
      /range _, lineItemsItem := \$body\.input\.\w+\?\['lineItems'\]/,
    );
    expect(transform).toContain("{{lineItemsItem?['itemSku']}}");
    expect(transform).not.toContain('range _, line-items');
  });

  it('ranges a root response array over $body through item', async () => {
    const action = await generateAction(arrayOas, 'createOrders');

    expect(action.responseTransforms).toContain('{{ range _, item := $body}}');
    expect(action.responseTransforms).toContain("{{item?['unit_count']}}");
  });

  it('never uses a keyword, an invalid identifier or `item` as a loop variable', async () => {
    const action = await generateAction(arrayOas, 'createOrders');
    const variables = [
      ...action.responseTransforms.matchAll(/range _, (\S+) :=/g),
    ].map((match) => match[1]);

    expect(variables).toEqual(
      expect.arrayContaining([
        'item',
        'endItem',
        'item_2faCodesItem',
        'itemItem',
      ]),
    );
    variables.forEach((variable) =>
      expect(variable).toMatch(/^[A-Za-z][A-Za-z0-9_]*$/),
    );
    // only the root array binds `item`
    expect(variables.filter((variable) => variable === 'item')).toHaveLength(1);
  });

  it('renders an array of arrays as nested ranges', async () => {
    const action = await generateAction(arrayOas, 'createOrders');

    expect(action.responseTransforms).toContain(
      "\"eventGrid\": {{if inverse(empty(item?['event_grid']))}} {{ range _, eventGridItem := item?['event_grid']}} " +
        '{{if inverse(empty(eventGridItem))}} {{ range _, eventGridItemItem := eventGridItem}} {',
    );
    expect(action.responseTransforms).toContain(
      '"atTime": {{eventGridItemItem?[\'at_time\']}}',
    );
    expect(action.responseTransforms).not.toContain('ARRAY');
  });

  it('only writes response keys that are fields of the generated type', async () => {
    const action = await generateAction(arrayOas, 'createOrders');
    const typeFields = new Set(
      [...action.types.matchAll(/^\s+(\w+)(?:\(.*\))?:/gm)].map(
        (match) => match[1],
      ),
    );
    const keys = [...action.responseTransforms.matchAll(/"([^"]+)":/g)].map(
      (match) => match[1],
    );

    // `2fa_codes` is named `_2faCodes` in the GraphQL schema
    expect(keys).toContain('_2faCodes');
    keys.forEach((key) => expect(typeFields).toContain(key));
  });

  it('reads request fields under their GraphQL names', async () => {
    const action = await generateAction(arrayOas, 'createOrders');
    const transform = action.requestTransforms?.value as string;

    expect(transform).toContain(
      "{{ range _, item_2faCodesItem := lineItemsItem?['_2faCodes']}}",
    );
  });
});
