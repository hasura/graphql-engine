import {
  createTableRelationshipRequestBody,
  deleteTableRelationshipRequestBody,
} from './utils';

describe('createTableRelationshipRequestBody [CREATE LOCAL RELATIONSHIPS]:', () => {
  it(`1
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Local FK relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_array_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          foreign_key_constraint_on: ['ArtistId'],
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Local FK relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_array_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          foreign_key_constraint_on: ['ArtistId'],
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`3
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> Remote relationship hack for same table
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`4
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: no
  Output --> Throws an error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual('Local Relationship not supported');
    }
  });

  it(`5
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Manual local relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_array_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          manual_configuration: {
            remote_table: { name: 'Artist', schema: 'public' },
            column_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`6
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Manual local relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_array_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          manual_configuration: {
            remote_table: { name: 'Artist', schema: 'public' },
            column_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`7
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> Remote relationship hack for same table
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`8
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: no
  Output ---> Throw Error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual('Local Relationship not supported');
    }
  });

  it(`9
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Local FK relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_object_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          foreign_key_constraint_on: ['ArtistId'],
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`10
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output ---> FK relationship as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_object_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          foreign_key_constraint_on: ['ArtistId'],
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`11
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output ---> Remote relationship hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`12
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: no
  Output ---> throws Error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual('Local Relationship not supported');
    }
  });

  it(`13
  target: localDB
  type: object
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output ---> Manual Local rel as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_object_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          manual_configuration: {
            remote_table: { name: 'Artist', schema: 'public' },
            column_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`14
  target: localDB
  type: object
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output ---> Manual Local rel as per docs
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_object_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
        source: 'chinook',
        using: {
          manual_configuration: {
            remote_table: { name: 'Artist', schema: 'public' },
            column_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`15
  target: localDB
  type: object
  fk_constraint: no
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output ---> Remote relationship hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`16
  target: localDB
  type: object
  fk_constraint: no
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: no
  Output ---> Throw Error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual('Local Relationship not supported');
    }
  });
});

describe('createTableRelationshipRequestBody [EDIT LOCAL RELATIONSHIPS]:', () => {
  it(`1
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`2
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`3
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> update remote rel hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
      isEditMode: true,
    });

    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`4
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`5
  target: localDB
  type: array
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`6
  target: localDB
  type: array
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> update remote rel hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
      isEditMode: true,
    });

    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`7
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`8
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            fkConstraintOn: 'fromTable',
            fromColumns: ['ArtistId'],
            toColumns: ['ArtistId'],
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`9
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> update remote rel hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          fkConstraintOn: 'fromTable',
          fromColumns: ['ArtistId'],
          toColumns: ['ArtistId'],
        },
      },
      isEditMode: true,
    });

    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`10
  target: localDB
  type: object
  fk_constraint: no
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: yes
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`11
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: yes
  isRemoteDBRelationshipSupported: no
  Output --> Throws Error
`, () => {
    expect(() =>
      createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: true,
          isRemoteSchemaRelationshipSupported: true,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toSource: 'chinook',
            toTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            columnMapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
        isEditMode: true,
      })
    ).toThrowError(Error('Edit Local Relationship not supported'));
  });

  it(`12
  target: localDB
  type: object
  fk_constraint: yes
  isLocalDBRelationshipSupported: no
  isRemoteDBRelationshipSupported: yes
  Output --> update remote rel hack
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toSource: 'chinook',
          toTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
      isEditMode: true,
    });

    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });
});

describe('createTableRelationshipRequestBody [CREATE REMOTE DATABASE RELATIONSHIPS]', () => {
  it(`1
  target: remoteDB
  type: array
  isRemoteDBRelationshipSupported: yes
  Output ---> Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: false,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSource: 'another_chinook',
          toRemoteTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: { ArtistId: 'ArtistId' },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'another_chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: remoteDB
  type: array
  isRemoteDBRelationshipSupported: no
  Output ---> throw Error 
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSource: 'another_chinook',
            toRemoteTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            columnMapping: { ArtistId: 'ArtistId' },
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Database Relationship not supported'
      );
    }
  });

  it(`3
  target: remoteDB
  type: object
  isRemoteDBRelationshipSupported: yes
  Output ---> Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: false,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSource: 'another_chinook',
          toRemoteTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: { ArtistId: 'ArtistId' },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'another_chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`4
  target: remoteDB
  type: object
  isRemoteDBRelationshipSupported: no
  Output ---> throw error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSource: 'another_chinook',
            toRemoteTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            columnMapping: { ArtistId: 'ArtistId' },
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Database Relationship not supported'
      );
    }
  });
});

describe('createTableRelationshipRequestBody [EDIT REMOTE DATABASE RELATIONSHIPS]', () => {
  it(`1
  target: remoteDB
  type: array
  isRemoteDBRelationshipSupported: yes
  Output ---> Edit Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: false,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSource: 'another_chinook',
          toRemoteTable: { name: 'Artist', schema: 'public' },
        },
        type: 'array',
        detail: {
          columnMapping: { ArtistId: 'ArtistId' },
        },
      },
      isEditMode: true,
    });
    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'array',
            source: 'another_chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: remoteDB
  type: array
  isRemoteDBRelationshipSupported: no
  Output ---> throw Error 
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSource: 'another_chinook',
            toRemoteTable: { name: 'Artist', schema: 'public' },
          },
          type: 'array',
          detail: {
            columnMapping: { ArtistId: 'ArtistId' },
          },
        },
        isEditMode: true,
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Database Relationship not supported'
      );
    }
  });

  it(`3
  target: remoteDB
  type: object
  isRemoteDBRelationshipSupported: yes
  Output ---> Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: false,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: true,
        isRemoteTableRelationshipSupported: true,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSource: 'another_chinook',
          toRemoteTable: { name: 'Artist', schema: 'public' },
        },
        type: 'object',
        detail: {
          columnMapping: { ArtistId: 'ArtistId' },
        },
      },
      isEditMode: true,
    });
    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_source: {
            relationship_type: 'object',
            source: 'another_chinook',
            table: { name: 'Artist', schema: 'public' },
            field_mapping: {
              ArtistId: 'ArtistId',
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`4
  target: remoteDB
  type: object
  isRemoteDBRelationshipSupported: no
  Output ---> throw error
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: true,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSource: 'another_chinook',
            toRemoteTable: { name: 'Artist', schema: 'public' },
          },
          type: 'object',
          detail: {
            columnMapping: { ArtistId: 'ArtistId' },
          },
        },
        isEditMode: true,
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Database Relationship not supported'
      );
    }
  });
});

describe('createTableRelationshipRequestBody [CREATE REMOTE SCHEMA RELATIONSHIPS]', () => {
  it(`1
  target: remoteSchema
  isRemoteSchemaRelationshipShip: yes
  Output ---> Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSchema: 'test_remote_schema',
        },
        detail: {
          lhs_fields: ['ArtistId'],
          remote_field: {
            arguments: {
              ArtistId: '$ArtistId',
            },
          },
        },
      },
    });
    const expectedResult = {
      type: 'postgres_create_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_remote_schema: {
            remote_schema: 'test_remote_schema',
            lhs_fields: ['ArtistId'],
            remote_field: {
              arguments: {
                ArtistId: '$ArtistId',
              },
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: remoteSchema
  isRemoteSchemaRelationshipShip: no
  Output ---> throw Error 
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSchema: 'test_remote_schema',
          },
          detail: {
            lhs_fields: ['ArtistId'],
            remote_field: {
              arguments: {
                ArtistId: '$ArtistId',
              },
            },
          },
        },
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Schema Relationship not supported'
      );
    }
  });
});

describe('createTableRelationshipRequestBody [EDIT REMOTE SCHEMA RELATIONSHIPS]', () => {
  it(`1
  target: remoteSchema
  isRemoteSchemaRelationshipShip: yes
  Output ---> Remote rel as per doc
`, () => {
    const result = createTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: {
        fromSource: 'chinook',
        fromTable: { name: 'Album', schema: 'public' },
      },
      sourceCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      targetCapabilities: {
        isLocalTableRelationshipSupported: false,
        isRemoteTableRelationshipSupported: false,
        isRemoteSchemaRelationshipSupported: true,
      },
      definition: {
        target: {
          toRemoteSchema: 'test_remote_schema',
        },
        detail: {
          lhs_fields: ['ArtistId'],
          remote_field: {
            arguments: {
              ArtistId: '$ArtistId',
            },
          },
        },
      },
      isEditMode: true,
    });
    const expectedResult = {
      type: 'postgres_update_remote_relationship',
      args: {
        name: 'test_relationship',
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        definition: {
          to_remote_schema: {
            remote_schema: 'test_remote_schema',
            lhs_fields: ['ArtistId'],
            remote_field: {
              arguments: {
                ArtistId: '$ArtistId',
              },
            },
          },
        },
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: remoteSchema
  isRemoteSchemaRelationshipShip: no
  Output ---> throw Error 
`, () => {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = createTableRelationshipRequestBody({
        driver: 'postgres',
        name: 'test_relationship',
        source: {
          fromSource: 'chinook',
          fromTable: { name: 'Album', schema: 'public' },
        },
        sourceCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: false,
        },
        targetCapabilities: {
          isLocalTableRelationshipSupported: false,
          isRemoteTableRelationshipSupported: false,
          isRemoteSchemaRelationshipSupported: true,
        },
        definition: {
          target: {
            toRemoteSchema: 'test_remote_schema',
          },
          detail: {
            lhs_fields: ['ArtistId'],
            remote_field: {
              arguments: {
                ArtistId: '$ArtistId',
              },
            },
          },
        },
        isEditMode: true,
      });
    } catch (e: any) {
      expect(e.message).toStrictEqual(
        'Remote Schema Relationship not supported'
      );
    }
  });
});

describe('deleteRelationshipRequestBody [DELETE RELATIONSHIP]', () => {
  it(`1
  target: localRelationship
  Output ---> drop local relationship 
`, () => {
    const result = deleteTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: 'chinook',
      table: { name: 'Album', schema: 'public' },
      isRemote: false,
    });
    const expectedResult = {
      type: 'postgres_drop_relationship',
      args: {
        table: { name: 'Album', schema: 'public' },
        source: 'chinook',
        relationship: 'test_relationship',
      },
    };
    expect(result).toStrictEqual(expectedResult);
  });

  it(`2
  target: remoteRelationship
  Output ---> drop remote relationship 
`, () => {
    const result = deleteTableRelationshipRequestBody({
      driver: 'postgres',
      name: 'test_relationship',
      source: 'chinook',
      table: { name: 'Album', schema: 'public' },
      isRemote: true,
    });
    const expectedResult = {
      type: 'postgres_delete_remote_relationship',
      args: {
        source: 'chinook',
        table: { name: 'Album', schema: 'public' },
        name: 'test_relationship',
      },
    };

    expect(result).toStrictEqual(expectedResult);
  });
});
