import { EventDataNode } from 'antd/lib/tree';

export type HasuraRsFields = string[];

export type AllowedRootFields = ('query' | 'mutation' | 'subscription')[];

export type ArgValueKind = 'field' | 'static';

export type ArgValue = {
  kind: ArgValueKind;
  value: string | number | boolean;
  type: string;
};

export type RelationshipFields = {
  key: string;
  argValue: ArgValue | null;
  checkable: boolean;
  depth: number;
  type: 'field' | 'arg';
};

export type TreeNode = {
  title: JSX.Element | string;
  key: string;
  checkable: boolean;
  depth: number;
  type: 'field' | 'arg';
  disabled?: boolean;
  argValue?: ArgValue | null;
  children?: TreeNode[];
};

export type RemoteField = {
  [FieldName: string]: {
    arguments: InputArgumentsType | never;
    field?: RemoteField;
  };
};

export type InputArgumentValueType =
  | string
  | boolean
  | number
  | InputArgumentsType;

export type InputArgumentsType = {
  [key: string]: InputArgumentValueType;
};

// we should extend RemoteRelationship type from `metadata/types.ts` once we have
// the correct types in place, for both old and new format
export type RemoteRelationship = {
  name: string;
  definition: {
    to_remote_schema: {
      lhs_fields: string[];
      remote_field: RemoteField;
      remote_schema: string;
    };
  };
};

export interface AntdTreeNode extends EventDataNode {
  title: JSX.Element | string;
  key: string;
  checkable: boolean;
  depth: number;
  type: 'field' | 'arg';
  disabled?: boolean;
  argValue?: ArgValue | null;
  children?: TreeNode[];
}
