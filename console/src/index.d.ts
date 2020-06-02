// HGE common types

export type RunSqlType = {
  type: string;
  args: {
    cascade?: boolean;
    read_only?: boolean;
    sql: string;
  };
};
