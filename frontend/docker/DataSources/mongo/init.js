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

db.createCollection('students', {
  validator: {
    $jsonSchema: {
      bsonType: 'object',
      title: 'Student Object Validation',
      required: ['address', 'gpa', 'name', 'year'],
      properties: {
        name: {
          bsonType: 'string',
          description: "'name' must be a string and is required",
        },
        year: {
          bsonType: 'int',
          minimum: 2017,
          maximum: 3017,
          description:
            "'year' must be an integer in [ 2017, 3017 ] and is required",
        },
        gpa: {
          bsonType: ['double'],
          description: "'gpa' must be a double if the field exists",
        },
        address: {
          bsonType: ['object'],
          properties: {
            city: { bsonType: 'string' },
            street: { bsonType: 'string' },
          },
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

db.students.insertMany([
  {
    name: 'Stu',
    year: 2018,
    gpa: 3.5,
    address: {
      city: 'Moon',
      street: 'Asteroid 6',
    },
  },
  {
    name: 'Stan',
    year: 2020,
    gpa: 5.01,
    address: {
      city: 'Mars',
      street: 'Volcano 10',
    },
  },
]);
