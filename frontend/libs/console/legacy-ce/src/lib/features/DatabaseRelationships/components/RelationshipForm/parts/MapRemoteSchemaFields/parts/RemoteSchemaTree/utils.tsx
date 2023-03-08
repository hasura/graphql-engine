import React from 'react';
import {
  isInputObjectType,
  isInterfaceType,
  isObjectType,
  GraphQLSchema,
  GraphQLField,
  GraphQLType,
  GraphQLArgument,
  GraphQLInputField,
  isWrappingType,
  isListType,
  isNonNullType,
} from 'graphql';
import {
  isEmpty,
  isFloat,
  isJsonString,
  isNumber,
} from '../../../../../../../../components/Common/utils/jsUtils';
import {
  AllowedRootFields,
  ArgValue,
  HasuraRsFields,
  RelationshipFields,
  TreeNode,
  RemoteRelationship,
  RemoteField,
  InputArgumentsType,
  InputArgumentValueType,
  AntdTreeNode,
} from '../../types';
import { SubFieldTitle } from './components/SubFieldTitle';
import { RootFieldTitle } from './components/RootFieldTitle';
import { FieldLabel } from './components/FieldLabel';
import { ArgFieldTitle } from './components/ArgFieldTitle';

export const getFieldData = (nodeData: AntdTreeNode): RelationshipFields => ({
  key: nodeData.key,
  depth: nodeData.depth,
  checkable: nodeData.checkable,
  argValue: nodeData.argValue ?? null,
  type: nodeData.type,
});

export const defaultArgValue: ArgValue = {
  kind: 'field',
  value: '',
  type: 'String',
};

export const findRemoteField = (
  fields: RelationshipFields[],
  field: AntdTreeNode
) => {
  return fields.find(f => f.key === field.key);
};

const isElementActive = (
  relationshipFields: RelationshipFields[],
  fieldKey: string
) => {
  return relationshipFields.some(f => f.key === fieldKey);
};

export const getUnderlyingType = (_type: Record<string, any>) => {
  let type = Object.assign(Object.create(_type), _type);
  const wraps = [];
  while (isWrappingType(type)) {
    if (isListType(type)) wraps.push('l');
    if (isNonNullType(type)) wraps.push('n');
    type = type.ofType;
  }
  return {
    wraps,
    type,
  };
};

/* returns checked value if arg is checked
 * returns null if arg isn't involved in the relationship
 */
export const getCheckedArgValue = (
  relationshipFields: RelationshipFields[],
  key: string
): ArgValue | null => {
  const field = relationshipFields.find(r => r.key === key);
  if (field) {
    return field.argValue;
  }
  return null;
};

const getPlaceholderChild = (parentKey: string, depth: number): TreeNode => ({
  title: '',
  key: `${parentKey}.__placeholder.${depth}`,
  depth: depth + 1,
  checkable: false,
  type: 'field',
  disabled: true,
});

const buildArgElement = ({
  arg,
  parentKey,
  relationshipFields,
  setRelationshipFields,
  fieldOptions,
  depth,
}: {
  arg: GraphQLArgument | GraphQLInputField;
  parentKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  fieldOptions: HasuraRsFields;
  depth: number;
}): TreeNode => {
  const { type: argType }: { type: GraphQLType } = getUnderlyingType(arg.type);
  let children: TreeNode[] = [];
  let checkable = true;
  const argKey = `${parentKey}.${arg.name}`;
  const isActive = isElementActive(relationshipFields, argKey);
  const argValue = getCheckedArgValue(relationshipFields, argKey);

  if (isInputObjectType(argType) || isInterfaceType(argType)) {
    const argFields = argType.getFields();
    if (!isEmpty(argFields)) {
      checkable = false;
      children = [getPlaceholderChild(argKey, depth + 1)];
      if (isActive) {
        children = [
          ...Object.values(argFields).map(argField =>
            buildArgElement({
              arg: argField,
              parentKey: argKey,
              relationshipFields,
              setRelationshipFields,
              fieldOptions,
              depth: depth + 1,
            })
          ),
        ];
      }
    }
  }

  return {
    title: (
      <ArgFieldTitle
        title={arg.name}
        argKey={argKey}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        fields={fieldOptions}
        showForm={isActive && checkable}
        argValue={argValue || defaultArgValue}
        argType={argType}
      />
    ),
    key: argKey,
    depth,
    type: 'arg',
    checkable,
    argValue: checkable ? argValue || defaultArgValue : null,
    children,
  };
};

interface BuildFieldElementArgs {
  field: GraphQLField<any, any, Record<string, any>>;
  parentKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  depth: number;
  isSubfield: boolean;
  fieldOptions: HasuraRsFields;
}

const buildFieldElement = ({
  field,
  parentKey,
  relationshipFields,
  setRelationshipFields,
  fieldOptions,
  depth,
  isSubfield,
}: BuildFieldElementArgs): TreeNode => {
  const { type: fieldType }: { type: GraphQLType } = getUnderlyingType(
    field.type
  );
  const fieldKey = `${parentKey}.${field.name}`;
  const enabled =
    (field.args && !!field.args.length) || isObjectType(fieldType);

  let children: TreeNode[] = [];
  if (enabled) {
    children = [getPlaceholderChild(fieldKey, depth + 1)];
  }

  const isActive = isElementActive(relationshipFields, fieldKey);
  if (isActive) {
    children = [];

    if (field.args && !!field.args.length) {
      children = [
        {
          title: <FieldLabel title="Arguments" />,
          key: `${fieldKey}.__placeholder.args.${depth}`,
          checkable: false,
          type: 'field',
          depth,
        },
        ...field.args.map(arg =>
          buildArgElement({
            arg,
            parentKey: `${fieldKey}.arguments`,
            relationshipFields,
            setRelationshipFields,
            fieldOptions,
            depth: depth + 1,
          })
        ),
      ];
    }
    if (isObjectType(fieldType) || isInterfaceType(fieldType)) {
      const subFields = fieldType.getFields();
      if (!isEmpty(subFields)) {
        children = [
          ...children,
          {
            title: <FieldLabel title="Sub-Fields" />,
            key: `${fieldKey}.__placeholder.sub_fields.${depth}`,
            checkable: false,
            type: 'field',
            depth,
          },
          ...Object.values(subFields).map(subField =>
            buildFieldElement({
              field: subField,
              parentKey: `${fieldKey}.field`,
              relationshipFields,
              setRelationshipFields,
              fieldOptions,
              depth: depth + 1,
              isSubfield: true,
            })
          ),
        ];
      }
    }
  }

  return {
    title: (
      <SubFieldTitle
        title={field.name}
        enabled={enabled}
        isSubfield={isSubfield}
      />
    ),
    key: fieldKey,
    depth: depth + 1,
    checkable: false,
    type: 'field',
    disabled: !enabled,
    children,
  };
};

interface BuildTreeArgs {
  schema: GraphQLSchema;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  rootFields: AllowedRootFields;
  fields: HasuraRsFields;
}

export const buildTree = ({
  schema,
  relationshipFields,
  setRelationshipFields,
  fields: fieldOptions,
  rootFields,
}: BuildTreeArgs): TreeNode[] => {
  const treeData: TreeNode[] = [];
  if (rootFields.includes('query')) {
    const queryType = schema.getQueryType();
    const fields = queryType?.getFields();
    const fieldKey = '__query';
    if (fields) {
      treeData.push({
        title: <RootFieldTitle title="Query" />,
        key: fieldKey,
        checkable: false,
        depth: 0,
        type: 'field',
        children: Object.values(fields).map(field =>
          buildFieldElement({
            field,
            parentKey: `${fieldKey}.field`,
            relationshipFields,
            setRelationshipFields,
            fieldOptions,
            depth: 0,
            isSubfield: false,
          })
        ),
      });
    }
  }
  if (rootFields.includes('mutation')) {
    const mutationType = schema.getMutationType();
    const fields = mutationType?.getFields();
    const fieldKey = '__mutation';
    if (fields) {
      treeData.push({
        title: <RootFieldTitle title="Mutation" />,
        key: fieldKey,
        checkable: false,
        depth: 0,
        type: 'field',
        children: Object.values(fields).map(field =>
          buildFieldElement({
            field,
            parentKey: `${fieldKey}.field`,
            relationshipFields,
            setRelationshipFields,
            fieldOptions,
            depth: 0,
            isSubfield: false,
          })
        ),
      });
    }
  }
  if (rootFields.includes('subscription')) {
    const subscriptionType = schema.getSubscriptionType();
    const fields = subscriptionType?.getFields();
    const fieldKey = '__subscription';
    if (fields) {
      treeData.push({
        title: <RootFieldTitle title="Subscription" />,
        key: fieldKey,
        checkable: false,
        depth: 0,
        type: 'field',
        children: Object.values(fields).map(field =>
          buildFieldElement({
            field,
            parentKey: `${fieldKey}.field`,
            relationshipFields,
            setRelationshipFields,
            fieldOptions,
            depth: 0,
            isSubfield: false,
          })
        ),
      });
    }
  }
  return treeData;
};

const getRemoteFieldObject = (
  ukSplit: string[][],
  depth: number,
  maxDepth: number
): RemoteField | InputArgumentsType | undefined => {
  if (depth > maxDepth) {
    return;
  }
  const obj: RemoteField | InputArgumentsType = {};
  const depthRemoteFields: string[] = ukSplit.map(uk => uk[depth] ?? '');

  // get unique depth remote fields at current depth
  const uniqueDepthRemoteFields = Array.from(new Set(depthRemoteFields));
  uniqueDepthRemoteFields.forEach((uniqueField, i) => {
    if (uniqueField.length > 0) {
      const newUkSplit: string[][] = [];
      depthRemoteFields.forEach((depthField, index) => {
        if (depthField === uniqueField) {
          newUkSplit.push(ukSplit[index]);
        }
      });

      // if leaf, push arg value
      if (ukSplit[i][depth + 1] === '__argVal') {
        const value = ukSplit[i][depth + 2];
        if (value.startsWith('__SCALAR__'))
          obj[uniqueField] = isJsonString(value.substring(10))
            ? JSON.parse(value.substring(10))
            : '';
        else obj[uniqueField] = value;
      } else {
        obj[uniqueField] = {
          ...getRemoteFieldObject(newUkSplit, depth + 1, maxDepth),
        };
        if (ukSplit[i][depth - 1] === 'field') {
          obj[uniqueField] = {
            ...(obj[uniqueField] as RemoteField),
            arguments: {
              ...(obj[uniqueField] as RemoteField).arguments,
            },
          };
        }
      }
    }
  });
  return Object.keys(obj).length ? obj : undefined;
};

const getUniqueRelFields = (relationshipFields: RelationshipFields[]) => {
  return relationshipFields?.filter(
    f =>
      !relationshipFields.some(
        refF => refF.key !== f.key && refF.key.includes(f.key)
      )
  );
};

const getKeysWithArgValues = (relationshipFields: RelationshipFields[]) =>
  relationshipFields?.map(field => {
    if (field.type === 'arg') {
      if (field.argValue && field.argValue?.kind === 'static')
        return `${field.key}.__argVal.${field.argValue?.value}`;
      else if (field.argValue && field.argValue?.kind === 'field')
        return `${field.key}.__argVal.$${field.argValue?.value}`;
      return `${field.key}.__argVal.$`;
    }
    return field.key;
  });

export const buildServerRemoteFieldObject = (
  relationshipFields: RelationshipFields[]
) => {
  const uniqueRelFields = getUniqueRelFields(relationshipFields);
  const uniqueKeys = getKeysWithArgValues(uniqueRelFields);
  const ukSplit: string[][] = [];
  uniqueKeys.forEach(uk => ukSplit.push(uk.split('.')));
  let maxDepth = 0;
  ukSplit.forEach(ar => {
    if (ar.length > maxDepth) maxDepth = ar.length;
  });

  const relationshipObject = getRemoteFieldObject(ukSplit, 2, maxDepth);
  return (relationshipObject as RemoteField) || {};
};

export const getExpandedKeys = (relationshipFields: RelationshipFields[]) =>
  relationshipFields.filter(rf => !rf.argValue).map(rf => rf.key);

export const getCheckedKeys = (relationshipFields: RelationshipFields[]) =>
  relationshipFields.filter(rf => rf.argValue).map(rf => rf.key);

export const parseArgValue = (
  argValue: InputArgumentValueType
): ArgValue | null => {
  if (typeof argValue === 'object' && argValue !== null) {
    return null;
  }
  if (typeof argValue === 'string') {
    const isStatic = !argValue.startsWith('$');
    return {
      value: isStatic ? argValue.toString() : argValue.substr(1),
      kind: isStatic ? 'static' : 'field',
      type: 'String',
    };
  }
  if (typeof argValue === 'boolean') {
    return {
      kind: 'static',
      value: argValue.toString(),
      type: 'Boolean',
    };
  }
  return {
    kind: 'static',
    value: argValue.toString(),
    type:
      (isNumber(argValue) && (isFloat(argValue) ? 'Float' : 'Int')) || 'String',
  };
};

const serialiseArguments = (
  args: InputArgumentValueType,
  key: string,
  depth: number,
  callback: (f: RelationshipFields) => void
): void => {
  if (typeof args === 'object') {
    Object.keys(args).forEach(argName => {
      const argValue = args[argName];
      const argValueMetadata = parseArgValue(argValue);
      if (argValueMetadata) {
        callback({
          key: `${key}.${argName}`,
          depth,
          checkable: true,
          argValue: argValueMetadata,
          type: 'arg',
        });
      } else {
        callback({
          key: `${key}.${argName}`,
          depth,
          checkable: false,
          argValue: null,
          type: 'arg',
        });
        serialiseArguments(argValue, `${key}.${argName}`, depth + 1, callback);
      }
    });
  }
};

const serialiseRemoteField = (
  field: {
    arguments: InputArgumentsType | never;
    field?: RemoteField | undefined;
  },
  key: string,
  depth: number,
  callback: (f: RelationshipFields) => void
): void => {
  callback({
    key,
    depth,
    checkable: false,
    argValue: null,
    type: 'field',
  });
  if (field.field) {
    const subFieldName = field.field ? Object.keys(field.field)[0] : '';
    const subField = field.field?.[subFieldName];
    serialiseRemoteField(
      subField,
      `${key}.field.${subFieldName}`,
      depth + 1,
      callback
    );
  }
  if (field.arguments) {
    serialiseArguments(
      field.arguments,
      `${key}.arguments`,
      depth + 1,
      callback
    );
  }
};

// TODO: this only parses the remote relationship in old format, and the type `RemoteRelationship` is old format
// we should extend this for both old & new format once the server work is done, and remove this comment
export const parseServerRelationship = (
  serverRelationship: RemoteRelationship
): RelationshipFields[] => {
  const remoteFields =
    serverRelationship?.definition?.to_remote_schema.remote_field;
  if (!remoteFields || isEmpty(remoteFields)) {
    return [];
  }
  // only query root is supported by server relationships, so we only expect fields under query root
  // this could be extended to use mutation and subscription if server support exists in future
  const key = '__query';
  const depth = 0;
  const relationshipFields: RelationshipFields[] = [
    { key, depth, checkable: false, argValue: null, type: 'field' },
  ];

  Object.keys(remoteFields).forEach(rf => {
    serialiseRemoteField(
      remoteFields[rf],
      `${key}.field.${rf}`,
      depth + 1,
      (field: RelationshipFields) => relationshipFields.push(field)
    );
  });
  return relationshipFields;
};
