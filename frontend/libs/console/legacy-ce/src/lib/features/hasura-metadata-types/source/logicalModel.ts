export type LogicalModelField = {
  name: string;
  nullable: boolean;
  type: string;
};

export type LogicalModel = {
  fields: LogicalModelField[];
  name: string;
};
