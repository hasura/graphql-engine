export type NativeQueryArgument = {
  type: string;
  default_value?: string;
  required?: boolean;
};

export type NativeQuery = {
  root_field_name: string;
  code: string;
  returns: string;
  arguments?: Record<string, NativeQueryArgument>;
  type?: 'query' | 'mutation'; // only query supported for now
  comment?: string;
};
