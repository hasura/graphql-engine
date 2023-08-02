import { Oas3 } from '@hasura/open-api-to-graphql';
import { ParameterObject } from '@hasura/open-api-to-graphql';
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
      'findPets'
    );
    expect(action).toEqual({
      operationId: 'findPets',
      actionType: 'query',
      action:
        'type Query {\n  findPets(limit: Int, tags: [String]): [Pet]\n}\n',
      types:
        'scalar BigInt\n\ntype Pet {\n  id: BigInt!\n  name: String!\n  tag: String\n}\n',
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
        2
      ),
      headers: [],
      queryParams:
        '{{ concat ([concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }}), "limit={{$body.input?.limit}}&"]) }}',
    });
  });

  it('throws an error if the OAS is invalid', async () => {
    await expect(
      generateAction({} as unknown as Oas3, 'findPets')
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
      '{{ concat (["status={{$body.input?.status}}&", concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }})]) }}'
    );
  });
  it('should generate query params with one non-array param and one array param (reversed)', async () => {
    const queryParams = await generateQueryParams([tags, status]);
    expect(queryParams).toBe(
      '{{ concat ([concat({{ range _, x := $body.input?.tags }} "tags={{x}}&" {{ end }}), "status={{$body.input?.status}}&"]) }}'
    );
  });
});

describe('optimizer API', () => {
  it('should generate correctly', async () => {
    const action = await generateAction(
      optimizer as unknown as Oas3,
      'optimise_optimise_post'
    );
    expect(action).toMatchSnapshot();
  });
});
