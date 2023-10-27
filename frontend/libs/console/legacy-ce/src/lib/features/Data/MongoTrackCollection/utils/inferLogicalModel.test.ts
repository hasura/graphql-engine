import { inferLogicalModels } from './inferLogicalModel';

describe('inferLogicalModels', () => {
  it('returns a logical model', () => {
    const document = {
      _id: {
        $oid: '11123-123-123',
      },
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
      _id: {
        $oid: '11123-123-123',
      },
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
                logical_model: 'documents_addresses',
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
        name: 'documents_addresses',
      },
    ]);
  });

  it('returns multiple logical models with object', () => {
    const document = {
      _id: {
        $oid: 'asd',
      },
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
              logical_model: 'documents_address',
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
        name: 'documents_address',
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

  it('sanitizes the logical model name', () => {
    const document = {
      _id: {
        $oid: '64df83ef6561310e38a0985e',
      },
    };

    const logicalModels = inferLogicalModels(
      'new-documents',
      JSON.stringify(document)
    );

    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
        ],
        name: 'newdocuments',
      },
    ]);
  });

  it('manages documents with scalar arrays', () => {
    const document = {
      _id: {
        $oid: '5ed3d12427017764d14e3284',
      },
      images: [
        'img-1590939937048-1T2A9759.jpeg',
        'img-1590939938094-1T2A9700.jpeg',
        'img-1590939939011-1T2A9656.jpeg',
        'img-1590939939889-1T2A9651.jpeg',
      ],
      __v: 0,
    };

    const logicalModels = inferLogicalModels(
      'new-documents',
      JSON.stringify(document)
    );

    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          {
            name: 'images',
            type: { array: { nullable: false, scalar: 'string' } },
          },
          { name: '__v', type: { nullable: false, scalar: 'int' } },
        ],
        name: 'newdocuments',
      },
    ]);
  });

  it('handles documents with object ids with names other than _id', () => {
    const document = {
      _id: {
        $oid: '5a9427648b0beebeb69579cc',
      },
      name: 'Andrea Le',
      email: 'andrea_le@fakegmail.com',
      movie_id: {
        $oid: '573a1390f29313caabcd418c',
      },
      text: 'Rem officiis eaque repellendus amet eos doloribus. Porro dolor voluptatum voluptates neque culpa molestias. Voluptate unde nulla temporibus ullam.',
      date: {
        $date: '2012-03-26T23:20:16.000Z',
      },
    };

    const logicalModels = inferLogicalModels(
      'new-documents',
      JSON.stringify(document)
    );

    expect(logicalModels).toEqual([
      {
        fields: [
          { name: '_id', type: { nullable: false, scalar: 'objectId' } },
          { name: 'name', type: { nullable: false, scalar: 'string' } },
          { name: 'email', type: { nullable: false, scalar: 'string' } },
          {
            name: 'movie_id',
            type: { nullable: false, scalar: 'objectId' },
          },
          { name: 'text', type: { nullable: false, scalar: 'string' } },
          { name: 'date', type: { nullable: false, scalar: 'date' } },
        ],
        name: 'newdocuments',
      },
    ]);
  });
});
