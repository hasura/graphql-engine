import { LogicalModel, NativeQuery, Source } from '@hasura/shared/types';

export type NativeQueryWithSource = NativeQuery & { source: Source };
export type LogicalModelWithSource = LogicalModel & { source: Source };
