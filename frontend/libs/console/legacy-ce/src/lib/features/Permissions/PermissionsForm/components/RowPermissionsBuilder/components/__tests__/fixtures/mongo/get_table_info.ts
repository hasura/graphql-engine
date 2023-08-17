export default {
  name: ['students'],
  type: 'table',
  columns: [
    {
      name: 'name',
      type: 'string',
      nullable: false,
      description: "'name' must be a string and is required",
      insertable: true,
      updatable: true,
    },
    {
      name: 'year',
      type: 'int',
      nullable: false,
      description:
        "'year' must be an integer in [ 2017, 3017 ] and is required",
      insertable: true,
      updatable: true,
    },
    {
      name: 'gpa',
      type: 'double',
      nullable: false,
      description: "'gpa' must be a double if the field exists",
      insertable: true,
      updatable: true,
    },
    {
      name: 'address',
      type: {
        type: 'object',
        name: 'students_address',
      },
      nullable: false,
      insertable: true,
      updatable: true,
    },
    {
      name: '_id',
      type: 'objectId',
      nullable: false,
      description: 'primary key _id',
      insertable: false,
      updatable: false,
    },
  ],
  logical_models: [
    {
      name: 'students_address',
      fields: [
        {
          name: 'city',
          type: {
            nullable: true,
            scalar: 'string',
          },
        },
        {
          name: 'street',
          type: {
            nullable: true,
            scalar: 'string',
          },
        },
      ],
      description: 'generated from MongoDB validation schema',
    },
  ],
  primary_key: ['_id'],
  insertable: true,
  updatable: true,
  deletable: true,
};
