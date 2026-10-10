export type NativeQueryArgument = {
  type: string;
  description?: string;
  nullable?: boolean;
};

export type NativeQueryRelationship = {
  name: string;
  using: {
    column_mapping: Record<string, string>;
    insertion_order: 'before_parent' | 'after_parent' | null;
    remote_native_query: string;
  };
};

export type NativeQuery = {
  root_field_name: string;
  code: string;
  returns: string;
  arguments?: Record<string, NativeQueryArgument>;
  type?: 'query' | 'mutation'; // only query supported for now
  comment?: string;
  object_relationships?: NativeQueryRelationship[];
  array_relationships?: NativeQueryRelationship[];
};
