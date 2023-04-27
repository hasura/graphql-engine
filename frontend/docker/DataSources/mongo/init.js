db.createCollection('mycollection', {
  validator: {
    $jsonSchema: {
      bsonType: 'object',
      required: ['name', 'age'],
      properties: {
        name: {
          bsonType: 'string',
          description: 'must be a string and is required',
        },
        age: {
          bsonType: 'int',
          minimum: 18,
          description: 'must be an integer and is required',
        },
        email: {
          bsonType: 'string',
          pattern: '^.+@.+$',
          description:
            'must be a string and match the regular expression pattern',
        },
      },
    },
  },
});

db.mycollection.insertMany([
  {
    name: 'John',
    age: 30,
    email: 'john@example.com',
  },
  {
    name: 'Jane',
    age: 25,
    email: 'jane@example.com',
  },
]);
