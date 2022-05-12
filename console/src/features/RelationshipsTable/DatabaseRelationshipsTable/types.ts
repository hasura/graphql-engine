import {
  DbToDbRelationship,
  DbToRemoteSchemaRelationship,
} from '@/features/MetadataAPI';

export interface RowData {
  fromType: 'database';
  toType: 'remote_schema' | 'database';
  name: string;
  source: string;
  destination: string;
  type: string;
  fieldsFrom: string[];
  fieldsTo: string[];
  relationship: DbToDbRelationship | DbToRemoteSchemaRelationship;
}
