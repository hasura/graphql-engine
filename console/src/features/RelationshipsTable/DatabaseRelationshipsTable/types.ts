import {
  DbToDbRelationship,
  DbToRemoteSchemaRelationship,
} from '@/features/MetadataAPI';
import { ArrayRelationship, ObjectRelationship } from '@/metadata/types';

export interface RowData {
  fromType: 'database' | 'table';
  toType: 'remote_schema' | 'database' | 'table';
  name: string;
  reference: string;
  referenceTable: string;
  target: string;
  targetTable?: string;
  type: string;
  fieldsFrom: string[];
  fieldsTo: string[];
  relationship:
    | DbToDbRelationship
    | DbToRemoteSchemaRelationship
    | ObjectRelationship
    | ArrayRelationship;
}
