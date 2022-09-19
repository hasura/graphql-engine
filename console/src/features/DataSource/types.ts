import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToSourceRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
  Table,
  SupportedDrivers,
  Source,
} from '@/features/MetadataAPI';

import { NetworkArgs } from './api';

export type Ref = { $ref: string };

export type OneOf = { oneOf: (Property | Ref)[]; description?: string };

export const isFreeFormObjectField = (
  property: Property & { type: 'object' }
): property is Property & {
  type: 'object';
  additionalProperties: true;
} => {
  if (!('additionalProperties' in property)) return false;

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const additionalProperties = property.additionalProperties;

  return true;
};

export type Property = {
  description?: string;
  nullable?: boolean;
} & (
  | {
      type: 'object';
      properties: Record<string, Ref | Property | OneOf>;
    }
  | {
      type: 'object';
      additionalProperties: true;
    }
  | {
      type: 'string';
      enum?: string[];
    }
  | {
      type: 'number';
    }
  | {
      type: 'boolean';
    }
  | {
      type: 'array';
      items:
        | { type: 'string' | 'number' }
        | {
            type: 'object';
            properties: Record<string, Ref | Property | OneOf>;
          }
        | {
            type: 'object';
            additionalProperties: true;
            nullable?: boolean;
          }
        | Ref;
    }
);

export type AllowedTableRelationships =
  | Legacy_SourceToRemoteSchemaRelationship
  | SourceToRemoteSchemaRelationship
  | SourceToSourceRelationship
  | ManualObjectRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship
  | ManualArrayRelationship
  | LocalTableArrayRelationship;

export type IntrospectedTable = {
  name: string;
  table: Table;
  type: string;
};

export type TableColumn = {
  name: string;
  dataType: string;
};

export type GetTrackableTablesProps = {
  dataSourceName: string;
  configuration: any;
} & NetworkArgs;
export type GetTableColumnsProps = {
  dataSourceName: string;
  configuration?: Source['configuration'];
  table: Table;
} & NetworkArgs;
export type GetFKRelationshipProps = {
  dataSourceName: string;
  table: Table;
} & NetworkArgs;

export type TableFkRelationships = {
  from: {
    table: string;
    column: string[];
  };
  to: {
    table: string;
    column: string[];
  };
};

export type GetTablesListAsTreeProps = {
  dataSourceName: string;
} & NetworkArgs;

type ReleaseType = 'GA' | 'Beta';

export type DriverInfoResponse = {
  name: SupportedDrivers;
  displayName: string;
  release: ReleaseType;
};

export type GetTableRowsProps = {
  table: Table;
  dataSourceName: string;
  columns: string[];
  options?: {
    where?: WhereClause;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
} & NetworkArgs;
export type TableRow = Record<string, unknown>;

export type validOperators =
  | '$eq'
  | '$ne'
  | '$in'
  | '$nin'
  | '$gt'
  | '$lt'
  | '$gte'
  | '$lte';

export type AndExp = Record<'$and', BoolExp[]>;
export type OrExp = Record<'$or', BoolExp[]>;
export type NotExp = Record<'$not', BoolExp[]>;
export type ColumnExpValue = Record<validOperators | string, any>;
export type ColumnExp = Record<string, ColumnExpValue>;
export type BoolExp = AndExp | OrExp | NotExp | ColumnExp;
export type SelectColumn = string | { name: string; columns: SelectColumn[] };
export type WhereClause = BoolExp | Record<string, any>;
export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';
export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls?: OrderByNulls;
};

export { NetworkArgs };
