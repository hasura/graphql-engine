import { Table } from '@/features/DataSource';

type SourceDef = {
  source: string;
  table: Table;
  columns: string[];
};

type RemoteSchemaDef = {
  remoteSchema: string;
  fields: string[];
};

export type Relationship = {
  name: string;
} & (
  | {
      type: 'toLocalTableManual' | 'toLocalTableFk';
      toLocalTable: Table;
      relationship_type: 'Array' | 'Object';
      mapping: {
        from: SourceDef;
        to: SourceDef;
      };
    }
  | {
      type: 'toSameTableFk';
      toLocalTable: Table;
      relationship_type: 'Object';
      mapping: {
        from: SourceDef;
        to: SourceDef;
      };
    }
  | {
      type: 'toSource';
      toSource: string;
      relationship_type: 'array' | 'object';
      mapping: {
        from: SourceDef;
        to: SourceDef;
      };
    }
  | {
      type: 'toRemoteSchema';
      toRemoteSchema: string;
      relationship_type: 'Remote Schema';
      mapping: {
        from: SourceDef;
        to: RemoteSchemaDef;
      };
    }
);
