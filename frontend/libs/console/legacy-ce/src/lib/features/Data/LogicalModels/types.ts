import { LogicalModel, NativeQuery, Source } from '../../hasura-metadata-types';

export type NativeQueryWithSource = NativeQuery & { source: Source };
export type LogicalModelWithSource = LogicalModel & { source: Source };

export type LogicalModelTabs = 'details' | 'permissions';

export type NativeQueryTabs = 'details' | 'relationships';
