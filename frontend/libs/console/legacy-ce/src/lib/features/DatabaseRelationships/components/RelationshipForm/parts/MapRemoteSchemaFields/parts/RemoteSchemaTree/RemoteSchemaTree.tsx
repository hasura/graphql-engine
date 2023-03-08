import React, { useMemo, useState } from 'react';
import { Tree as AntTree } from 'antd';
import { GraphQLSchema } from 'graphql';
import { EventDataNode } from 'antd/lib/tree';
import './index.css';
import {
  AllowedRootFields,
  AntdTreeNode,
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
import { SearchBar } from '../SearchBar/SearchBar';

export interface RemoteSchemaTreeProps
  extends React.ComponentProps<typeof AntTree> {
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
  ...rest
}: RemoteSchemaTreeProps) => {
  const defaultSearchValue = relationshipFields[1]
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
    [relationshipFields, schema, rootFields, fields]
  );

  const expandedKeys = useMemo(
    () => getExpandedKeys(relationshipFields),
    [relationshipFields]
  );

  const checkedKeys = useMemo(
    () => getCheckedKeys(relationshipFields),
    [relationshipFields]
  );

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

  const expandedParentTrees: string[] = [];
  const filteredTree = tree.map(subTree => {
    return {
      ...subTree,
      children: (subTree.children ?? []).filter(subTreeItem => {
        if (searchText.length) {
          if (subTreeItem.key.includes(searchText))
            expandedParentTrees.push(subTreeItem.key.split('.')[0]);
          return subTreeItem.key.includes(searchText);
        }
        return true;
      }),
    };
  });

  // If nothing can be opened via search, at least open the query root fields
  const uniqueExpandedRoots = !expandedParentTrees.length
    ? ['__query']
    : Array.from(new Set([...expandedParentTrees]));

  return (
    <div>
      <div className="mb-sm">
        <SearchBar
          value={searchText}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
            setSearchText(e.target.value)
          }
        />
      </div>

      <AntTree
        checkable
        checkStrictly
        blockNode
        selectable={false}
        onCheck={onCheck}
        onExpand={onExpand}
        treeData={filteredTree}
        expandedKeys={[...expandedKeys, ...uniqueExpandedRoots]}
        checkedKeys={checkedKeys}
        // disable animation onExpand to improve performance
        motion={null}
        {...rest}
      />
    </div>
  );
};
