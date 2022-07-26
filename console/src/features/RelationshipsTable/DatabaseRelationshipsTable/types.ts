import {
  DbToDbRelationship,
  DbToRemoteSchemaRelationship,
} from '@/features/MetadataAPI';
import { ArrayRelationship, ObjectRelationship } from '@/metadata/types';

type BaseTypes = {
  name: string;
  reference: string;
  referenceTable: string;
  target: string;
  targetTable?: string;
  fieldsFrom: string[];
  fieldsTo: string[];
};

type DbToDb = {
  fromType: 'table';
  toType: 'database';
  type: 'Object' | 'Array';
  relationship: DbToDbRelationship;
} & BaseTypes;

type LocalObject = {
  fromType: 'table';
  toType: 'table';
  type: 'Object';
  relationship: ObjectRelationship;
};

type LocalArray = {
  fromType: 'table';
  toType: 'table';
  type: 'Array';
  relationship: ArrayRelationship;
};

type Remote = {
  fromType: 'table';
  toType: 'remote_schema';
  type: 'Remote Schema';
  relationship: DbToRemoteSchemaRelationship;
};

export type RowData = (DbToDb | LocalObject | LocalArray | Remote) & BaseTypes;
