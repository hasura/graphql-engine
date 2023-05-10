import { LogicalModel, NativeQuery } from '../../hasura-metadata-types';

export type NativeQueryWithSource = NativeQuery & { source: string };
export type LogicalModelWithSource = LogicalModel & { source: string };
