export type ScalarFieldType = {
  scalar: string;
  nullable: boolean;
};

// indicates a field that is in the shape of another logical model -- i.e. nested logical model
export type LogicalModelType = {
  logical_model: string;
  nullable: boolean;
};

// indicates that the field returns an array of the referecned logical model
export type ArrayLogicalModelType = {
  array: LogicalModelType;
};

export type ArrayScalarFieldType = {
  array: ScalarFieldType;
};

export type LogicalModelField = {
  name: string;
  type:
    | ScalarFieldType
    | LogicalModelType
    | ArrayLogicalModelType
    | ArrayScalarFieldType;
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
