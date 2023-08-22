import { inferLogicalModels } from './inferLogicalModel';

describe('inferLogicalModels', () => {
  it('returns a logical model', () => {
    const document = {
      _id: '11123-123-123',
      name: 'John',
      age: 30,
      isActive: true,
    };

    const logicalModels = inferLogicalModels(
      'documents',
      JSON.stringify(document)
    );
    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name', type: { nullable: false, scalar: 'string' } },
          { name: 'age', type: { nullable: false, scalar: 'int' } },
          { name: 'isActive', type: { nullable: false, scalar: 'bool' } },
        ],
        name: 'documents',
      },
    ]);
  });

  it('returns multiple logical models with array', () => {
    const document = {
      _id: '11123-123-123',
      name: 'John',
      age: 30,
      isActive: true,
      addresses: [
        {
          street: '123 Main St',
          city: 'New York',
          state: 'NY',
        },
        {
          street: '123 Main St',
          city: 'New York',
          state: 'NY',
          number: 123,
        },
      ],
    };

    const logicalModels = inferLogicalModels(
      'documents',
      JSON.stringify(document)
    );
    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name', type: { nullable: false, scalar: 'string' } },
          { name: 'age', type: { nullable: false, scalar: 'int' } },
          { name: 'isActive', type: { nullable: false, scalar: 'bool' } },
          {
            name: 'addresses',
            type: {
              array: {
                logical_model: 'addresses',
                nullable: false,
              },
            },
          },
        ],
        name: 'documents',
      },
      {
        fields: [
          {
            name: 'street',
            type: {
              nullable: false,
              scalar: 'string',
            },
          },
          {
            name: 'city',
            type: {
              nullable: false,
              scalar: 'string',
            },
          },
          {
            name: 'state',
            type: {
              nullable: false,
              scalar: 'string',
            },
          },
          {
            name: 'number',
            type: {
              nullable: true,
              scalar: 'int',
            },
          },
        ],
        name: 'addresses',
      },
    ]);
  });

  it('returns multiple logical models with object', () => {
    const document = {
      _id: 'asd',
      name: 'Stu',
      year: 2018,
      gpa: 3.5,
      address: {
        city: 'Moon',
        street: 'Asteroid 6',
      },
    };

    const logicalModels = inferLogicalModels(
      'documents',
      JSON.stringify(document)
    );
    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name', type: { nullable: false, scalar: 'string' } },
          { name: 'year', type: { nullable: false, scalar: 'int' } },
          { name: 'gpa', type: { nullable: false, scalar: 'double' } },
          {
            name: 'address',
            type: {
              logical_model: 'address',
              nullable: false,
            },
          },
        ],
        name: 'documents',
      },
      {
        fields: [
          {
            name: 'city',
            type: {
              nullable: false,
              scalar: 'string',
            },
          },
          {
            name: 'street',
            type: {
              nullable: false,
              scalar: 'string',
            },
          },
        ],
        name: 'address',
      },
    ]);
  });

  it('correctly handle _id', () => {
    const document = {
      _id: {
        $oid: '64df83ef6561310e38a0985e',
      },
      name: 'Stu',
    };

    const logicalModels = inferLogicalModels(
      'documents',
      JSON.stringify(document)
    );

    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name', type: { nullable: false, scalar: 'string' } },
        ],
        name: 'documents',
      },
    ]);
  });

  it('sanitizes the key names to be GraphQL compliant', () => {
    const document = {
      _id: {
        $oid: '64df83ef6561310e38a0985e',
      },
      '!@#$%^&*()-=name1/': 'Stu',
    };

    const logicalModels = inferLogicalModels(
      'documents',
      JSON.stringify(document)
    );

    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name1', type: { nullable: true, scalar: 'string' } },
        ],
        name: 'documents',
      },
    ]);
  });
});
