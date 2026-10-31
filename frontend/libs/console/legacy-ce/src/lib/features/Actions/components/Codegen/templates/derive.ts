import {
  DefinitionNode,
  Kind,
  ObjectTypeDefinitionNode,
  OperationDefinitionNode,
  SelectionNode,
  TypeNode,
  parse,
} from 'graphql';
import {
  collectFields,
  getFieldSelection,
} from '../../../../../shared/utils/graphqlSchemaUtils';

// Returns the expression that renames the aliased fields of `value`, whose
// fields are selected by `selections`, to the fields of the action type
// `type`; or undefined when no field of `value` (or nested in it) is aliased.
const renameAliases = (
  value: string,
  type: TypeNode,
  selections: readonly SelectionNode[],
  definitions: readonly DefinitionNode[],
  indent: string,
  depth = 0,
): string | undefined => {
  if (type.kind !== Kind.NON_NULL_TYPE) {
    const renamed = renameAliases(
      value,
      { kind: Kind.NON_NULL_TYPE, type },
      selections,
      definitions,
      indent,
      depth,
    );
    return renamed && `${value} && ${renamed}`;
  }

  const nonNullType = type.type;
  if (nonNullType.kind === Kind.LIST_TYPE) {
    const row = depth ? `row${depth}` : 'row';
    const renamed = renameAliases(
      row,
      nonNullType.type,
      selections,
      definitions,
      indent,
      depth + 1,
    );
    return renamed && `${value}.map((${row}) => (${renamed}))`;
  }

  const objectType = definitions.find(
    (d): d is ObjectTypeDefinitionNode =>
      d.kind === Kind.OBJECT_TYPE_DEFINITION &&
      d.name.value === nonNullType.name.value,
  );
  if (!objectType) {
    return undefined;
  }

  let hasAliases = false;
  const fields = (objectType.fields ?? []).flatMap((field) => {
    const selection = getFieldSelection(selections, field.name.value);
    if (!selection) {
      return [];
    }

    const fieldValue = `${value}.${selection.responseKey}`;
    const renamed = renameAliases(
      fieldValue,
      field.type,
      selection.selections,
      definitions,
      `${indent}  `,
      depth,
    );
    if (renamed || selection.responseKey !== field.name.value) {
      hasAliases = true;
    }
    return [`${indent}  ${field.name.value}: ${renamed ?? fieldValue}`];
  });

  if (!hasAliases) {
    return undefined;
  }
  return `{
${fields.join(',\n')}
${indent}}`;
};

// Returns the JS expression that builds the action output from the `data` of
// the derived operation's response.
//
// The response is keyed by the aliases of the operation, e.g. for
// `query { users: user { userId: id } }` it is `{ users: [{ userId: 1 }] }`,
// while the action output type has the `id` field. So when any of the fields
// of the output type, or of its nested object types, is selected under an
// alias, the response is mapped back onto the field names of the action types
// defined in `definitions`.
export const getDerivedOutput = (
  operation: string,
  actionOutputType: TypeNode,
  definitions: readonly DefinitionNode[],
  indent = '  ',
): string => {
  const operationDef = parse(operation)
    .definitions[0] as OperationDefinitionNode;
  const rootField = collectFields(operationDef.selectionSet.selections).find(
    (f) => !f.name.value.startsWith('__'),
  );
  if (!rootField) {
    throw new Error('the derived operation must have a root field');
  }

  const response = `data.${(rootField.alias ?? rootField.name).value}`;
  return (
    renameAliases(
      response,
      actionOutputType,
      rootField.selectionSet?.selections ?? [],
      definitions,
      indent,
    ) ?? response
  );
};
