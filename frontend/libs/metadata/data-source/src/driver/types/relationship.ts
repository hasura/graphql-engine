import * as z from 'zod';
import { Table } from '@hasura/shared/types';
import { DataSourceNetworkArgs } from './types';

export type ForeignKeyTarget = {
  table: Table;
  columns: string[];
};

export type ModifyForeignKeyArgs = {
  constraintName: string;
  from: ForeignKeyTarget;
  to: ForeignKeyTarget;
  onUpdate?: ViolationAction;
  onDelete?: ViolationAction;
};

export type ModifyForeignKeyProps = {
  dataSourceName: string;
  isMigration: boolean;
} & DataSourceNetworkArgs &
  ModifyForeignKeyArgs;

export type CreateForeignKeyProps = Omit<
  ModifyForeignKeyProps,
  'constraintName'
> & {
  constraintName?: string;
};

export type GetFKRelationshipProps = {
  dataSourceName: string;
  table: Table;
} & DataSourceNetworkArgs;

export type TableFkRelationships = {
  name?: string;
  from: ForeignKeyTarget;
  to: ForeignKeyTarget;
  onUpdate?: ViolationAction;
  onDelete?: ViolationAction;
};

const violationActionSchema = z.enum([
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
]);

export type ViolationAction = z.infer<typeof violationActionSchema>;

export const foreignKeyFormSchema = z.object({
  referenceTable: z.unknown(),
  columnMappings: z.array(
    z.object({
      from: z.string(),
      to: z.string(),
    }),
  ),
  onUpdate: violationActionSchema.nullish(),
  onDelete: violationActionSchema.nullish(),
});

export type ForeignKeyFormSchema = z.infer<typeof foreignKeyFormSchema>;
