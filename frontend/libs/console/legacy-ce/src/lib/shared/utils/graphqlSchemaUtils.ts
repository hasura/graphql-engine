import {
  FieldNode,
  GraphQLInputObjectType,
  GraphQLInputFieldMap,
  GraphQLSchema,
  Kind,
  SelectionNode,
} from 'graphql';

export const getOperationType = (schema: GraphQLSchema, operation: string) => {
  if (operation === 'query') {
    return schema.getQueryType();
  }
  if (operation === 'subscription') {
    return schema.getSubscriptionType();
  }
  return schema.getMutationType();
};

export const getTypeFields = (
  _type: GraphQLInputObjectType,
): GraphQLInputFieldMap => {
  return _type.getFields() || {};
};

// Returns the fields of a selection set, including the ones selected through
// inline fragments (`... on user { id }`). Named fragment spreads are skipped,
// as their definitions are not part of the selection set.
export const collectFields = (
  selections: readonly SelectionNode[],
): FieldNode[] =>
  selections.flatMap((s) => {
    if (s.kind === Kind.FIELD) {
      return [s];
    }
    if (s.kind === Kind.INLINE_FRAGMENT) {
      return collectFields(s.selectionSet.selections);
    }
    return [];
  });

// Returns how the field `name` is selected in a selection set: the key of its
// value in the response (its alias, if any) and its sub-selections. When the
// field is selected under several aliases, the first one is used; the
// selections under the same response key are merged, as in the response.
export const getFieldSelection = (
  selections: readonly SelectionNode[],
  name: string,
): { responseKey: string; selections: SelectionNode[] } | undefined => {
  const fields = collectFields(selections).filter((f) => f.name.value === name);
  if (!fields.length) {
    return undefined;
  }

  const responseKey = (fields[0].alias ?? fields[0].name).value;
  return {
    responseKey,
    selections: fields
      .filter((f) => (f.alias ?? f.name).value === responseKey)
      .flatMap((f) => f.selectionSet?.selections ?? []),
  };
};
