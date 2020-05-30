export type ReduxAction = {
  type: string;
  payload: {
    pathname: string;
  };
  data: any;
};

export type GetReduxState = () => Record<string, any>;
