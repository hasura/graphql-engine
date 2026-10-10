import React, { useMemo, useState } from 'react';
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
  getExpandedKeys,
  getCheckedKeys,
} from './utils';
import { getFieldData } from '../../../../../RemoteRelationships/RemoteSchemaRelationships/components/RemoteSchemaTree/utils';
import { SearchInput, Tree, TreeProps } from '@hasura/shared/ui';

export interface RemoteSchemaTreeProps extends Pick<
  TreeProps<TreeNode>,
  'checkable' | 'className'
> {
  /**
   * Graphql schema for setting new permissions.
   */
  schema: GraphQLSchema;
  relationshipFields: RelationshipFields[];
  rootFields: AllowedRootFields;
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  fields: HasuraRsFields;
}

export const RemoteSchemaTree = ({
  schema,
  relationshipFields,
  rootFields,
  setRelationshipFields,
  fields,
  checkable = true,
  className,
}: RemoteSchemaTreeProps) => {
  const defaultSearchValue = relationshipFields?.[1]
    ? relationshipFields[1].key.split('.')[
        relationshipFields[1].key.split('.').length - 1
      ]
    : '';

  const [searchText, setSearchText] = useState(defaultSearchValue);

  const tree: TreeNode[] = useMemo(
    () =>
      buildTree({
        schema,
        relationshipFields,
        setRelationshipFields,
        fields,
        rootFields,
      }),
    [relationshipFields, schema, rootFields, fields],
  );

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

  const expandedParentTrees: string[] = [];
  const filteredTree = tree.map((subTree) => {
    return {
      ...subTree,
      children: (subTree.children ?? []).filter((subTreeItem) => {
        if (searchText.length) {
          if (subTreeItem.key.includes(searchText))
            expandedParentTrees.push(subTreeItem.key.split('.')[0]);
          return subTreeItem.key.includes(searchText);
        }
        return true;
      }),
    };
  });

  const uniqueExpandedRoots = Array.from(new Set([...expandedParentTrees]));

  return (
    <div>
      <div className="mb-2">
        <SearchInput value={searchText} onSearch={setSearchText} />
      </div>

      <Tree
        checkable={checkable}
        blockNode
        onCheck={onCheck}
        onExpand={onExpand}
        treeData={filteredTree}
        expandedKeys={[...expandedKeys, ...uniqueExpandedRoots]}
        checkedKeys={checkedKeys}
        className={className}
      />
    </div>
  );
};
