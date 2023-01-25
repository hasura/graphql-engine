export type TableObject = {
  name: string;
  schema: string;
};

export type ForeignKeyMap = {
  table: string;
  column: string[];
};

export type ForeignKeyMapping = {
  to: ForeignKeyMap;
  from: ForeignKeyMap;
};
