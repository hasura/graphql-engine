export type ScalarFieldType = {
  scalar: string;
  nullable: boolean;
};

export type LogicalModelType = {
  logical_model: string;
  nullable: boolean;
};

export type ArrayLogicalModelType = {
  array: LogicalModelType;
};

export type LogicalModelField = {
  name: string;
  type: ScalarFieldType | LogicalModelType | ArrayLogicalModelType;
};

export type LogicalModel = {
  fields: LogicalModelField[];
  name: string;
  select_permissions?: {
    permission: {
      columns: string[];
      filter: Record<string, any>;
    };
    role: string;
  }[];
};
