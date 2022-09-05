import { Relationship } from '../types';

export const expectedRemoteSchemaRelationshipOutput: Relationship & {
  type: 'toRemoteSchema';
} = {
  name: 'new_rs_to_db',
  type: 'toRemoteSchema',
  toRemoteSchema: 'rs',
  relationship_type: 'Remote Schema',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
    to: {
      remoteSchema: 'rs',
      fields: ['country'],
    },
  },
};

export const expectedLegacyRemoteSchemaRelationshipsOutput: Relationship & {
  type: 'toRemoteSchema';
} = {
  name: 'legacy_db_to_rs',
  type: 'toRemoteSchema',
  toRemoteSchema: 'rs',
  relationship_type: 'Remote Schema',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
    to: {
      remoteSchema: 'rs',
      fields: ['country'],
    },
  },
};

export const expectedRemoteDBRelationshipOutput: Relationship & {
  type: 'toSource';
} = {
  name: 'remote_db_object_rel',
  type: 'toSource',
  toSource: 'bikes',
  relationship_type: 'object',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
    to: {
      source: 'bikes',
      table: {
        name: 'brands',
        schema: 'production',
      },
      columns: ['brand_id'],
    },
  },
};

export const expectedManualLocalRelationshipOutput: Relationship & {
  type: 'toLocalTableManual';
} = {
  name: 'local_array_rel',
  type: 'toLocalTableManual',
  toLocalTable: {
    name: 'Album',
    schema: 'public',
  },
  relationship_type: 'Object',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
    to: {
      source: 'chinook',
      table: {
        name: 'Album',
        schema: 'public',
      },
      columns: ['AlbumId'],
    },
  },
};

export const expectedLocalTableRelationships: Relationship & {
  type: 'toLocalTableFk';
} = {
  name: 'Employees',
  type: 'toLocalTableFk',
  toLocalTable: {
    name: 'Employee',
    schema: 'public',
  },
  relationship_type: 'Object',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['ReportsTo'],
    },
    to: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
  },
};

export const expectedSameTableObjectRelationships: Relationship & {
  type: 'toSameTableFk';
} = {
  name: 'Employee',
  type: 'toSameTableFk',
  toLocalTable: {
    name: 'Employee',
    schema: 'public',
  },
  relationship_type: 'Object',
  mapping: {
    from: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['ReportsTo'],
    },
    to: {
      source: 'chinook',
      table: {
        name: 'Employee',
        schema: 'public',
      },
      columns: ['EmployeeId'],
    },
  },
};
