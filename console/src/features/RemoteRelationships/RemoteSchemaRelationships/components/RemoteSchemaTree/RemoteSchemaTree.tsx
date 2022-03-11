import React, { useMemo } from 'react';
import 'antd/dist/antd.css';
import { Tree as AntTree } from 'antd';
import { GraphQLSchema } from 'graphql';
import { EventDataNode } from 'antd/lib/tree';
import {
  AllowedRootFields,
  AntdTreeNode,
  HasuraColumn,
  RelationshipFields,
  TreeNode,
} from './types';
import {
  buildTree,
  findRemoteField,
  getFieldData,
  getExpandedKeys,
  getCheckedKeys,
} from './utils';

export interface RemoteSchemaTreeProps {
  /**
   * Graphql schema for setting new permissions.
   */
  schema: GraphQLSchema;
  relationshipFields: RelationshipFields[];
  rootFields: AllowedRootFields;
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  /**
   * Columns array from the current table.
   */
  columns: HasuraColumn;
}

export const RemoteSchemaTree = ({
  schema,
  relationshipFields,
  rootFields,
  setRelationshipFields,
  columns,
}: RemoteSchemaTreeProps) => {
  const tree: TreeNode[] = useMemo(
    () =>
      buildTree(
        schema,
        relationshipFields,
        setRelationshipFields,
        columns,
        rootFields
      ),
    [relationshipFields, schema, rootFields, columns]
  );

  const expandedKeys = useMemo(() => getExpandedKeys(relationshipFields), [
    relationshipFields,
  ]);

  const checkedKeys = useMemo(() => getCheckedKeys(relationshipFields), [
    relationshipFields,
  ]);

  const onCheck = (
    // onCheck props expects checked param
    checked:
      | React.Key[]
      | {
          checked: React.Key[];
          halfChecked: React.Key[];
        },
    // CheckInfo is not exported by the library
    // https://github.com/react-component/tree/issues/411
    checkedNodeInfo: Record<string, any>
  ) => {
    const nodeInfo = checkedNodeInfo.node as AntdTreeNode;
    const selectedField = findRemoteField(relationshipFields, nodeInfo);
    const fieldData = getFieldData(nodeInfo);

    if (selectedField) {
      setRelationshipFields(
        relationshipFields.filter(field => !(field.key === nodeInfo.key))
      );
    } else {
      setRelationshipFields([
        ...relationshipFields.filter(field => !(field.key === nodeInfo.key)),
        fieldData,
      ]);
    }
  };

  const onExpand = (
    expanded: React.Key[],
    expandedNodeInfo: {
      node: EventDataNode;
      expanded: boolean;
      nativeEvent: MouseEvent;
    }
  ) => {
    const nodeInfo = expandedNodeInfo.node as AntdTreeNode;
    const selectedField = findRemoteField(relationshipFields, nodeInfo);
    const fieldData = getFieldData(nodeInfo);
    if (selectedField) {
      // if the node is already expanded, collapse the node,
      // and remove all its children
      setRelationshipFields(
        relationshipFields.filter(
          field =>
            !(
              field.key === nodeInfo.key ||
              field.key.includes(`${nodeInfo.key}.`)
            )
        )
      );
    } else {
      // `fields` at same or higher depth, if the current node is `argument` we skip this
      const levelDepthFields =
        nodeInfo.type === 'field'
          ? relationshipFields
              .filter(
                field => field.type === 'field' && field.depth >= nodeInfo.depth
              )
              .map(field => field.key)
          : [];

      // remove all the fields and their children which are on same/higher depth, and add the current field
      // as one parent can have only one field at a certain depth
      setRelationshipFields([
        ...relationshipFields.filter(
          field =>
            !(
              field.key === nodeInfo.key ||
              // remove all current or higher depth fields and their children
              (nodeInfo.type === 'field' &&
                levelDepthFields.some(
                  refFieldKey =>
                    field.key === refFieldKey ||
                    field.key.includes(`${refFieldKey}.`)
                ))
            )
        ),
        fieldData,
      ]);
    }
  };

  return (
    <AntTree
      checkable
      checkStrictly
      blockNode
      selectable={false}
      onCheck={onCheck}
      onExpand={onExpand}
      treeData={tree}
      expandedKeys={expandedKeys}
      checkedKeys={checkedKeys}
      // disable animation onExpand to improve performance
      motion={null}
    />
  );
};
