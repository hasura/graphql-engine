import React, { useMemo } from 'react';
import { GraphQLSchema } from 'graphql';
import {
  AllowedRootFields,
  HasuraRsFields,
  RelationshipFields,
  TreeNode,
} from '../../types';
import {
  buildTree,
  findRemoteField,
  getFieldData,
  getExpandedKeys,
  getCheckedKeys,
} from './utils';
import { Tree, TreeProps } from '@hasura/shared/ui';

export interface RemoteSchemaTreeProps extends Pick<
  TreeProps<TreeNode>,
  'checkable' | 'className'
> {
  /**
   * Graphql schema for setting new permissions.
   */
  schema: GraphQLSchema;
  relationshipFields: RelationshipFields[];
  selectedOperation?: string;
  rootFields: AllowedRootFields;
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  fields: HasuraRsFields;
  showOnlySelectable?: boolean;
}

export const RemoteSchemaTree = ({
  schema,
  relationshipFields,
  rootFields,
  selectedOperation,
  setRelationshipFields,
  fields,
  showOnlySelectable = false,
  checkable = true,
  className,
}: RemoteSchemaTreeProps) => {
  const tree: TreeNode[] = useMemo(() => {
    let tree = buildTree({
      schema,
      relationshipFields,
      setRelationshipFields,
      fields,
      rootFields,
      showOnlySelectable,
    });
    if (selectedOperation) {
      const selectedOperationSubTree = tree[0].children?.find(
        (child) => child.key === `__query.field.${selectedOperation}`,
      );
      if (selectedOperationSubTree) {
        tree = [selectedOperationSubTree];
      }
    }
    return tree;
  }, [relationshipFields, schema, rootFields, fields, selectedOperation]);

  const expandedKeys = useMemo(
    () => getExpandedKeys(relationshipFields),
    [relationshipFields],
  );

  const checkedKeys = useMemo(
    () => getCheckedKeys(relationshipFields),
    [relationshipFields],
  );

  const onCheck = (nodeInfo: TreeNode) => {
    const selectedField = findRemoteField(relationshipFields, nodeInfo);
    const fieldData = getFieldData(nodeInfo);

    if (selectedField) {
      setRelationshipFields(
        relationshipFields.filter((field) => !(field.key === nodeInfo.key)),
      );
    } else {
      setRelationshipFields([
        ...relationshipFields.filter((field) => !(field.key === nodeInfo.key)),
        fieldData,
      ]);
    }
  };

  const onExpand = (nodeInfo: TreeNode) => {
    const selectedField = findRemoteField(relationshipFields, nodeInfo);
    const fieldData = getFieldData(nodeInfo);
    if (selectedField) {
      // if the node is already expanded, collapse the node,
      // and remove all its children
      setRelationshipFields(
        relationshipFields.filter(
          (field) =>
            !(
              field.key === nodeInfo.key ||
              field.key.includes(`${nodeInfo.key}.`)
            ),
        ),
      );
    } else {
      // `fields` at same or higher depth, if the current node is `argument` we skip this
      const levelDepthFields =
        nodeInfo.type === 'field'
          ? relationshipFields
              .filter(
                (field) =>
                  field.type === 'field' && field.depth >= nodeInfo.depth,
              )
              .map((field) => field.key)
          : [];

      // remove all the fields and their children which are on same/higher depth, and add the current field
      // as one parent can have only one field at a certain depth

      setRelationshipFields([
        ...relationshipFields.filter(
          (field) =>
            !(
              field.key === nodeInfo.key ||
              // remove all current or higher depth fields and their children
              (nodeInfo.type === 'field' &&
                levelDepthFields.some(
                  (refFieldKey) =>
                    field.key === refFieldKey ||
                    field.key.includes(`${refFieldKey}.`),
                ))
            ),
        ),
        fieldData,
      ]);
    }
  };

  return (
    <Tree
      checkable={checkable}
      onCheck={onCheck}
      onExpand={onExpand}
      onNodeClick={(node) => {
        if ((node.children?.length || 0) > 0) {
          onExpand(node);
        }
      }}
      treeData={tree}
      expandedKeys={expandedKeys}
      checkedKeys={checkedKeys}
      className={className}
    />
  );
};
