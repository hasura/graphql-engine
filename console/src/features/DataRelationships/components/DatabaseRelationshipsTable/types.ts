import {
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  Table,
} from '@/features/MetadataAPI';

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
      type: 'toLocalTableManual';
      toLocalTable: Table;
      relationship_type: 'Array' | 'Object';
      mapping: {
        from: SourceDef;
        to: SourceDef;
      };
      definition: ManualObjectRelationship | ManualArrayRelationship;
    }
  | {
      type: 'toLocalTableFk';
      toLocalTable: Table;
      relationship_type: 'Array' | 'Object';
      mapping: {
        from: SourceDef;
        to: SourceDef;
      };
      definition: LocalTableObjectRelationship | LocalTableArrayRelationship;
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
