export type ReduxAction = {
  type: string;
  payload: {
    pathname: string;
  };
  data: any;
};

// HGE common types
export type RunSqlType = {
  type: string;
  args: {
    cascade?: boolean;
    read_only?: boolean;
    sql: string;
  };
};

export type GetReduxState = () => Record<string, any>;
