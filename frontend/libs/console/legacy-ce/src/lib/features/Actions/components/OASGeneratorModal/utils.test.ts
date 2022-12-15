import { Oas3 } from 'openapi-to-graphql';
import petStore from './petstore.json';
import { getOperations, generateAction } from './utils';

describe('getOperations', () => {
  it('should return an array of operations', async () => {
    const operations = await getOperations(petStore as unknown as Oas3);
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
    await expect(getOperations({} as unknown as Oas3)).rejects.toThrow();
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
      requestTransforms: '',
      responseTransforms: '',
      sampleInput:
        '{\n' +
        '  "action": {\n' +
        '    "name": "findPets"\n' +
        '  },\n' +
        '  "input": {\n' +
        '    "limit": 10\n' +
        '  }\n' +
        '}',
      headers: [],
      queryParams: ['tags', 'limit'],
    });
  });

  it('throws an error if the OAS is invalid', async () => {
    await expect(
      generateAction({} as unknown as Oas3, 'findPets')
    ).rejects.toThrow();
  });
});
