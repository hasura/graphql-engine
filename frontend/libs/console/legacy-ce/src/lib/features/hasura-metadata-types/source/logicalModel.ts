export type LogicalModelField = {
  name: string;
  nullable: boolean;
  type: string;
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
