import React from 'react';
import {
  isInputObjectType,
  isInterfaceType,
  isListType,
  isNonNullType,
  isEnumType,
  isObjectType,
  isScalarType,
  isWrappingType,
} from 'graphql';
/* This function sets the styling to the way the relationship looks, for eg: article.id -> user.user_id */
export const getRelDef = relMeta => {
  const lcol =
    relMeta.lcol.length > 1
      ? '( ' + relMeta.lcol.join(', ') + ' )'
      : relMeta.lcol[0];
  const rcol =
    relMeta.rcol.length > 1
      ? '( ' + relMeta.rcol.join(', ') + ' )'
      : relMeta.rcol[0];

  return relMeta.isObjRel ? (
    <span>
      {relMeta.lTable} . {lcol} &nbsp;&rarr;&nbsp;
      {relMeta.rTable} . {rcol}
    </span>
  ) : (
    <span>
      {relMeta.rTable} . {rcol} &nbsp;&rarr;&nbsp;
      {relMeta.lTable} . {lcol}
    </span>
  );
};

/* Gets the complete list of relationships and converts it to a list of object, which looks like so :
[ { objRel: {objectRelationship}, arrRel: {arrayRelationship} } ] */
export const getObjArrRelList = relationships => {
  const objRels = relationships.filter(r => r.rel_type === 'object');
  const arrRels = relationships.filter(r => r.rel_type !== 'object');
  const requiredList = [];
  const length =
    objRels.length > arrRels.length ? objRels.length : arrRels.length;
  for (let i = 0; i < length; i++) {
    const objRel = objRels[i] ? objRels[i] : null;
    const arrRel = arrRels[i] ? arrRels[i] : null;

    requiredList.push({
      objRel,
      arrRel,
    });
  }
  return requiredList;
};

export const sampleSchema = {
  data: {
    __schema: {
      directives: [
        {
          args: [
            {
              name: 'if',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
          ],
          name: 'include',
          locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
          description: null,
        },
        {
          args: [
            {
              name: 'if',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
          ],
          name: 'skip',
          locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
          description: null,
        },
      ],
      queryType: { name: 'query_root' },
      subscriptionType: { name: 'subscription_root' },
      types: [
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'Boolean',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'CacheControlScope',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'PRIVATE',
              description: '',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'PUBLIC',
              description: '',
            },
          ],
          description: '',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'INTERFACE',
          possibleTypes: [{ kind: 'OBJECT', name: 'Message', ofType: null }],
          interfaces: null,
          name: 'Communication',
          enumValues: null,
          description: '',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
              },
              description: '',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'msg',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: '',
            },
          ],
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'Float',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'ID',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
              },
              description: '',
            },
            {
              name: 'name',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: '',
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'IncludeInpObj',
          enumValues: null,
          description: '',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'Int',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: '',
            },
            {
              name: 'gt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: '',
            },
            {
              name: 'lt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: '',
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'IntCompareObj',
          enumValues: null,
          description: '',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [
            { kind: 'INTERFACE', name: 'Communication', ofType: null },
          ],
          name: 'Message',
          enumValues: null,
          description: '',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
              },
              description: '',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'msg',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: '',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: '',
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'IntCompareObj',
                ofType: null,
              },
              description: '',
            },
            {
              name: 'name',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringCompareObj',
                ofType: null,
              },
              description: '',
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'MessageWhereInpObj',
          enumValues: null,
          description: '',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'Query',
          enumValues: null,
          description: '',
          fields: [
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'communications',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INTERFACE',
                  name: 'Communication',
                  ofType: null,
                },
              },
              description: '',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'hello',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: '',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
                  },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'message',
              type: { kind: 'OBJECT', name: 'Message', ofType: null },
              description: '',
            },
            {
              args: [
                {
                  name: 'includes',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'IncludeInpObj',
                    ofType: null,
                  },
                  description: '',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'MessageWhereInpObj',
                    ofType: null,
                  },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'messages',
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'OBJECT', name: 'Message', ofType: null },
              },
              description: '',
            },
          ],
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'String',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: '',
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'StringCompareObj',
          enumValues: null,
          description: '',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'Upload',
          enumValues: null,
          description: 'The `Upload` scalar type represents a file upload.',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__Directive',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'args',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__InputValue',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'description',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'locations',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: '__DirectiveLocation',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: '__DirectiveLocation',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'ARGUMENT_DEFINITION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'ENUM',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'ENUM_VALUE',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'FIELD',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'FIELD_DEFINITION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'FRAGMENT_DEFINITION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'FRAGMENT_SPREAD',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INLINE_FRAGMENT',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INPUT_FIELD_DEFINITION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INPUT_OBJECT',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INTERFACE',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'MUTATION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'OBJECT',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'QUERY',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'SCALAR',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'SCHEMA',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'SUBSCRIPTION',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'UNION',
              description: null,
            },
          ],
          description: null,
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__EnumValue',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'deprecationReason',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'description',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'isDeprecated',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__Field',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'args',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__InputValue',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'deprecationReason',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'description',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'isDeprecated',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'type',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__InputValue',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'defaultValue',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'description',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'type',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__Schema',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'directives',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__Directive',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'mutationType',
              type: { kind: 'OBJECT', name: '__Type', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'queryType',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'subscriptionType',
              type: { kind: 'OBJECT', name: '__Type', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'types',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
                  },
                },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: '__Type',
          enumValues: null,
          description: null,
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'description',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [
                {
                  name: 'includeDeprecated',
                  defaultValue: 'false',
                  type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'enumValues',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'OBJECT', name: '__EnumValue', ofType: null },
                },
              },
              description: null,
            },
            {
              args: [
                {
                  name: 'includeDeprecated',
                  defaultValue: 'false',
                  type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'fields',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'OBJECT', name: '__Field', ofType: null },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'inputFields',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__InputValue',
                    ofType: null,
                  },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'interfaces',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
                },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'kind',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'ENUM', name: '__TypeKind', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'ofType',
              type: { kind: 'OBJECT', name: '__Type', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'possibleTypes',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'OBJECT', name: '__Type', ofType: null },
                },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: '__TypeKind',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'ENUM',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INPUT_OBJECT',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'INTERFACE',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'LIST',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'NON_NULL',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'OBJECT',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'SCALAR',
              description: null,
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'UNION',
              description: null,
            },
          ],
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: '_eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_gt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_gte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_in',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
            {
              name: '_is_null',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_lt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_lte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_neq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_nin',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'boolean_comparison_exp',
          enumValues: null,
          description:
            "expression to compare columns of type boolean. All fields are combined with logical 'AND'.",
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'conflict_action',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'ignore',
              description: 'ignore the insert on this row',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'update',
              description: 'update the row with the given values',
            },
          ],
          description: 'conflict action',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'jsonb',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: '_contained_in',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: 'is the column contained in the given json value',
            },
            {
              name: '_contains',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description:
                'does the column contain the given json value at the top level',
            },
            {
              name: '_eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_gt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_gte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_has_key',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description:
                'does the string exist as a top-level key in the column',
            },
            {
              name: '_has_keys_all',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'SCALAR', name: 'String', ofType: null },
                },
              },
              description:
                'do all of these strings exist as top-level keys in the column',
            },
            {
              name: '_has_keys_any',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: { kind: 'SCALAR', name: 'String', ofType: null },
                },
              },
              description:
                'do any of these strings exist as top-level keys in the column',
            },
            {
              name: '_in',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              },
              description: null,
            },
            {
              name: '_is_null',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_lt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_lte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_neq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: '_nin',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'jsonb_comparison_exp',
          enumValues: null,
          description:
            "expression to compare columns of type jsonb. All fields are combined with logical 'AND'.",
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'mutation_root',
          enumValues: null,
          description: 'mutation root',
          fields: [
            {
              args: [
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'post_bool_exp',
                      ofType: null,
                    },
                  },
                  description: 'filter the rows which have to be deleted',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'delete_post',
              type: {
                kind: 'OBJECT',
                name: 'post_mutation_response',
                ofType: null,
              },
              description: 'delete data from the table: "post"',
            },
            {
              args: [
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'user_bool_exp',
                      ofType: null,
                    },
                  },
                  description: 'filter the rows which have to be deleted',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'delete_user',
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              description: 'delete data from the table: "user"',
            },
            {
              args: [
                {
                  name: 'objects',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'post_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  description: 'the rows to be inserted',
                },
                {
                  name: 'on_conflict',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_on_conflict',
                    ofType: null,
                  },
                  description: 'on conflict condition',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'insert_post',
              type: {
                kind: 'OBJECT',
                name: 'post_mutation_response',
                ofType: null,
              },
              description: 'insert data into the table: "post"',
            },
            {
              args: [
                {
                  name: 'objects',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'user_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  description: 'the rows to be inserted',
                },
                {
                  name: 'on_conflict',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_on_conflict',
                    ofType: null,
                  },
                  description: 'on conflict condition',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'insert_user',
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              description: 'insert data into the table: "user"',
            },
            {
              args: [
                {
                  name: '_set',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_set_input',
                    ofType: null,
                  },
                  description:
                    'sets the columns of the filtered rows to the given values',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'post_bool_exp',
                      ofType: null,
                    },
                  },
                  description: 'filter the rows which have to be updated',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'update_post',
              type: {
                kind: 'OBJECT',
                name: 'post_mutation_response',
                ofType: null,
              },
              description: 'update data of the table: "post"',
            },
            {
              args: [
                {
                  name: '_append',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_append_input',
                    ofType: null,
                  },
                  description:
                    'append existing jsonb value of filtered columns with new jsonb value',
                },
                {
                  name: '_delete_at_path',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_delete_at_path_input',
                    ofType: null,
                  },
                  description:
                    'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
                },
                {
                  name: '_delete_elem',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_delete_elem_input',
                    ofType: null,
                  },
                  description:
                    'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
                },
                {
                  name: '_delete_key',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_delete_key_input',
                    ofType: null,
                  },
                  description:
                    'delete key/value pair or string element. key/value pairs are matched based on their key value',
                },
                {
                  name: '_prepend',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_prepend_input',
                    ofType: null,
                  },
                  description:
                    'prepend existing jsonb value of filtered columns with new jsonb value',
                },
                {
                  name: '_set',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_set_input',
                    ofType: null,
                  },
                  description:
                    'sets the columns of the filtered rows to the given values',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'user_bool_exp',
                      ofType: null,
                    },
                  },
                  description: 'filter the rows which have to be updated',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'update_user',
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              description: 'update data of the table: "user"',
            },
          ],
        },
        {
          inputFields: null,
          kind: 'SCALAR',
          possibleTypes: null,
          interfaces: null,
          name: 'numeric',
          enumValues: null,
          description: null,
          fields: null,
        },
        {
          inputFields: [
            {
              name: '_eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_gt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_gte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_in',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
              },
              description: null,
            },
            {
              name: '_is_null',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_lt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_lte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_neq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: '_nin',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'numeric_comparison_exp',
          enumValues: null,
          description:
            "expression to compare columns of type numeric. All fields are combined with logical 'AND'.",
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'order_by',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'asc',
              description: 'in the ascending order, nulls last',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'asc_nulls_first',
              description: 'in the ascending order, nulls first',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'asc_nulls_last',
              description: 'in the ascending order, nulls last',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'desc',
              description: 'in the descending order, nulls first',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'desc_nulls_first',
              description: 'in the descending order, nulls first',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'desc_nulls_last',
              description: 'in the descending order, nulls last',
            },
          ],
          description: 'column ordering options',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post',
          enumValues: null,
          description: 'columns and relationships of "post"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'is_published',
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'title',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'userByUserId',
              type: { kind: 'OBJECT', name: 'user', ofType: null },
              description: 'An object relationship',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_aggregate',
          enumValues: null,
          description: 'aggregated selection of "post"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'aggregate',
              type: {
                kind: 'OBJECT',
                name: 'post_aggregate_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'nodes',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'post', ofType: null },
                  },
                },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_aggregate_fields',
          enumValues: null,
          description: 'aggregate fields of "post"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'avg',
              type: { kind: 'OBJECT', name: 'post_avg_fields', ofType: null },
              description: null,
            },
            {
              args: [
                {
                  name: 'columns',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: null,
                },
                {
                  name: 'distinct',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'count',
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'max',
              type: { kind: 'OBJECT', name: 'post_max_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'min',
              type: { kind: 'OBJECT', name: 'post_min_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev',
              type: {
                kind: 'OBJECT',
                name: 'post_stddev_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev_pop',
              type: {
                kind: 'OBJECT',
                name: 'post_stddev_pop_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev_samp',
              type: {
                kind: 'OBJECT',
                name: 'post_stddev_samp_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'sum',
              type: { kind: 'OBJECT', name: 'post_sum_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'var_pop',
              type: {
                kind: 'OBJECT',
                name: 'post_var_pop_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'var_samp',
              type: {
                kind: 'OBJECT',
                name: 'post_var_samp_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'variance',
              type: {
                kind: 'OBJECT',
                name: 'post_variance_fields',
                ofType: null,
              },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'avg',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_avg_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'count',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'max',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_max_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'min',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_min_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_stddev_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev_pop',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_stddev_pop_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev_samp',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_stddev_samp_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'sum',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_sum_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'var_pop',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_var_pop_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'var_samp',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_var_samp_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'variance',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_variance_order_by',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_aggregate_order_by',
          enumValues: null,
          description: 'order by aggregate values of table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'data',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'post_insert_input',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              name: 'on_conflict',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_on_conflict',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_arr_rel_insert_input',
          enumValues: null,
          description:
            'input type for inserting array relation for remote table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_avg_fields',
          enumValues: null,
          description: 'aggregate avg on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_avg_order_by',
          enumValues: null,
          description: 'order by avg() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: '_and',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'post_bool_exp',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: '_not',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_bool_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: '_or',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'post_bool_exp',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: 'id',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'numeric_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'is_published',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'boolean_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'text_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'userByUserId',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_bool_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'numeric_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'numeric_comparison_exp',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_bool_exp',
          enumValues: null,
          description:
            'Boolean expression to filter rows from the table "post". All fields are combined with a logical \'AND\'.',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'post_constraint',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'post_pkey',
              description: 'unique or primary key constraint',
            },
          ],
          description: 'unique or primary key constraints on table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'is_published',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: 'userByUserId',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_obj_rel_insert_input',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_insert_input',
          enumValues: null,
          description: 'input type for inserting data into table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_max_fields',
          enumValues: null,
          description: 'aggregate max on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'title',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_max_order_by',
          enumValues: null,
          description: 'order by max() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_min_fields',
          enumValues: null,
          description: 'aggregate min on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'title',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_min_order_by',
          enumValues: null,
          description: 'order by min() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_mutation_response',
          enumValues: null,
          description: 'response of any mutation on the table "post"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'affected_rows',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
              },
              description: 'number of affected rows by the mutation',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'returning',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'post', ofType: null },
                  },
                },
              },
              description: 'data of the affected rows by the mutation',
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'data',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'post_insert_input',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: 'on_conflict',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_on_conflict',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_obj_rel_insert_input',
          enumValues: null,
          description:
            'input type for inserting object relation for remote table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'constraint',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'ENUM', name: 'post_constraint', ofType: null },
              },
              description: null,
            },
            {
              name: 'update_columns',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'post_update_column',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_on_conflict',
          enumValues: null,
          description: 'on conflict condition type for table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'is_published',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'userByUserId',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_order_by',
          enumValues: null,
          description: 'ordering options when selecting data from "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'post_select_column',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'is_published',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'title',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              description: 'column name',
            },
          ],
          description: 'select columns of table "post"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'is_published',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: 'title',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_set_input',
          enumValues: null,
          description: 'input type for updating data in table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_stddev_fields',
          enumValues: null,
          description: 'aggregate stddev on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_stddev_order_by',
          enumValues: null,
          description: 'order by stddev() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_stddev_pop_fields',
          enumValues: null,
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_stddev_pop_order_by',
          enumValues: null,
          description: 'order by stddev_pop() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_stddev_samp_fields',
          enumValues: null,
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_stddev_samp_order_by',
          enumValues: null,
          description: 'order by stddev_samp() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_sum_fields',
          enumValues: null,
          description: 'aggregate sum on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_sum_order_by',
          enumValues: null,
          description: 'order by sum() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'post_update_column',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'is_published',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'title',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              description: 'column name',
            },
          ],
          description: 'update columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_var_pop_fields',
          enumValues: null,
          description: 'aggregate var_pop on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_var_pop_order_by',
          enumValues: null,
          description: 'order by var_pop() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_var_samp_fields',
          enumValues: null,
          description: 'aggregate var_samp on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_var_samp_order_by',
          enumValues: null,
          description: 'order by var_samp() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'post_variance_fields',
          enumValues: null,
          description: 'aggregate variance on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'views',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'user_id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'views',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'post_variance_order_by',
          enumValues: null,
          description: 'order by variance() on columns of table "post"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'query_root',
          enumValues: null,
          description: 'query root',
          fields: [
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'communications',
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INTERFACE',
                  name: 'Communication',
                  ofType: null,
                },
              },
              description: '',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'hello',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: '',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
                  },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'message',
              type: { kind: 'OBJECT', name: 'Message', ofType: null },
              description: '',
            },
            {
              args: [
                {
                  name: 'includes',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'IncludeInpObj',
                    ofType: null,
                  },
                  description: '',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'MessageWhereInpObj',
                    ofType: null,
                  },
                  description: '',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'messages',
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'OBJECT', name: 'Message', ofType: null },
              },
              description: '',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'post', ofType: null },
                  },
                },
              },
              description: 'fetch data from the table: "post"',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post_aggregate',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'post_aggregate',
                  ofType: null,
                },
              },
              description: 'fetch aggregated fields from the table: "post"',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
                  },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post_by_pk',
              type: { kind: 'OBJECT', name: 'post', ofType: null },
              description:
                'fetch data from the table: "post" using primary key columns',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'user_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'user_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'user', ofType: null },
                  },
                },
              },
              description: 'fetch data from the table: "user"',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'user_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'user_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_aggregate',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'user_aggregate',
                  ofType: null,
                },
              },
              description: 'fetch aggregated fields from the table: "user"',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
                  },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_by_pk',
              type: { kind: 'OBJECT', name: 'user', ofType: null },
              description:
                'fetch data from the table: "user" using primary key columns',
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'subscription_root',
          enumValues: null,
          description: 'subscription root',
          fields: [
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'post', ofType: null },
                  },
                },
              },
              description: 'fetch data from the table: "post"',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post_aggregate',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'post_aggregate',
                  ofType: null,
                },
              },
              description: 'fetch aggregated fields from the table: "post"',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
                  },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'post_by_pk',
              type: { kind: 'OBJECT', name: 'post', ofType: null },
              description:
                'fetch data from the table: "post" using primary key columns',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'user_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'user_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'user', ofType: null },
                  },
                },
              },
              description: 'fetch data from the table: "user"',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'user_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'user_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_aggregate',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'user_aggregate',
                  ofType: null,
                },
              },
              description: 'fetch aggregated fields from the table: "user"',
            },
            {
              args: [
                {
                  name: 'id',
                  defaultValue: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
                  },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_by_pk',
              type: { kind: 'OBJECT', name: 'user', ofType: null },
              description:
                'fetch data from the table: "user" using primary key columns',
            },
          ],
        },
        {
          inputFields: [
            {
              name: '_eq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_gt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_gte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_ilike',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_in',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
            {
              name: '_is_null',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
              description: null,
            },
            {
              name: '_like',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_lt',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_lte',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_neq',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_nilike',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_nin',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
            {
              name: '_nlike',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_nsimilar',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: '_similar',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'text_comparison_exp',
          enumValues: null,
          description:
            "expression to compare columns of type text. All fields are combined with logical 'AND'.",
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user',
          enumValues: null,
          description: 'columns and relationships of "user"',
          fields: [
            {
              args: [
                {
                  name: 'path',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'String', ofType: null },
                  description: 'JSON select path',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'address',
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'numeric', ofType: null },
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'postsByUserId',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'post', ofType: null },
                  },
                },
              },
              description: 'An array relationship',
            },
            {
              args: [
                {
                  name: 'distinct_on',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'post_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: 'distinct select on columns',
                },
                {
                  name: 'limit',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'limit the nuber of rows returned',
                },
                {
                  name: 'offset',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Int', ofType: null },
                  description: 'skip the first n rows. Use only with order_by',
                },
                {
                  name: 'order_by',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'post_order_by',
                        ofType: null,
                      },
                    },
                  },
                  description: 'sort the rows by one or more columns',
                },
                {
                  name: 'where',
                  defaultValue: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'post_bool_exp',
                    ofType: null,
                  },
                  description: 'filter the rows returned',
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'postsByUserId_aggregate',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'post_aggregate',
                  ofType: null,
                },
              },
              description: 'An aggregated array relationship',
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_aggregate',
          enumValues: null,
          description: 'aggregated selection of "user"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'aggregate',
              type: {
                kind: 'OBJECT',
                name: 'user_aggregate_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'nodes',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'user', ofType: null },
                  },
                },
              },
              description: null,
            },
          ],
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_aggregate_fields',
          enumValues: null,
          description: 'aggregate fields of "user"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'avg',
              type: { kind: 'OBJECT', name: 'user_avg_fields', ofType: null },
              description: null,
            },
            {
              args: [
                {
                  name: 'columns',
                  defaultValue: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'user_select_column',
                        ofType: null,
                      },
                    },
                  },
                  description: null,
                },
                {
                  name: 'distinct',
                  defaultValue: null,
                  type: { kind: 'SCALAR', name: 'Boolean', ofType: null },
                  description: null,
                },
              ],
              isDeprecated: false,
              deprecationReason: null,
              name: 'count',
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'max',
              type: { kind: 'OBJECT', name: 'user_max_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'min',
              type: { kind: 'OBJECT', name: 'user_min_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev',
              type: {
                kind: 'OBJECT',
                name: 'user_stddev_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev_pop',
              type: {
                kind: 'OBJECT',
                name: 'user_stddev_pop_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'stddev_samp',
              type: {
                kind: 'OBJECT',
                name: 'user_stddev_samp_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'sum',
              type: { kind: 'OBJECT', name: 'user_sum_fields', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'var_pop',
              type: {
                kind: 'OBJECT',
                name: 'user_var_pop_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'var_samp',
              type: {
                kind: 'OBJECT',
                name: 'user_var_samp_fields',
                ofType: null,
              },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'variance',
              type: {
                kind: 'OBJECT',
                name: 'user_variance_fields',
                ofType: null,
              },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'avg',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_avg_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'count',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'max',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_max_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'min',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_min_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_stddev_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev_pop',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_stddev_pop_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'stddev_samp',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_stddev_samp_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'sum',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_sum_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'var_pop',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_var_pop_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'var_samp',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_var_samp_order_by',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'variance',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_variance_order_by',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_aggregate_order_by',
          enumValues: null,
          description: 'order by aggregate values of table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_append_input',
          enumValues: null,
          description:
            'append existing jsonb value of filtered columns with new jsonb value',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'data',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'user_insert_input',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
            {
              name: 'on_conflict',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_on_conflict',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_arr_rel_insert_input',
          enumValues: null,
          description:
            'input type for inserting array relation for remote table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_avg_fields',
          enumValues: null,
          description: 'aggregate avg on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_avg_order_by',
          enumValues: null,
          description: 'order by avg() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: '_and',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'user_bool_exp',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: '_not',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_bool_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: '_or',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'user_bool_exp',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: 'address',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'jsonb_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'id',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'numeric_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'text_comparison_exp',
                ofType: null,
              },
              description: null,
            },
            {
              name: 'postsByUserId',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_bool_exp',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_bool_exp',
          enumValues: null,
          description:
            'Boolean expression to filter rows from the table "user". All fields are combined with a logical \'AND\'.',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'user_constraint',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'user_pkey',
              description: 'unique or primary key constraint',
            },
          ],
          description: 'unique or primary key constraints on table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: { kind: 'SCALAR', name: 'String', ofType: null },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_delete_at_path_input',
          enumValues: null,
          description:
            'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'Int', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_delete_elem_input',
          enumValues: null,
          description:
            'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_delete_key_input',
          enumValues: null,
          description:
            'delete key/value pair or string element. key/value pairs are matched based on their key value',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
            {
              name: 'postsByUserId',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_arr_rel_insert_input',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_insert_input',
          enumValues: null,
          description: 'input type for inserting data into table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_max_fields',
          enumValues: null,
          description: 'aggregate max on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_max_order_by',
          enumValues: null,
          description: 'order by max() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_min_fields',
          enumValues: null,
          description: 'aggregate min on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_min_order_by',
          enumValues: null,
          description: 'order by min() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_mutation_response',
          enumValues: null,
          description: 'response of any mutation on the table "user"',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'affected_rows',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
              },
              description: 'number of affected rows by the mutation',
            },
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'returning',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: { kind: 'OBJECT', name: 'user', ofType: null },
                  },
                },
              },
              description: 'data of the affected rows by the mutation',
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'data',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'user_insert_input',
                  ofType: null,
                },
              },
              description: null,
            },
            {
              name: 'on_conflict',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_on_conflict',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_obj_rel_insert_input',
          enumValues: null,
          description:
            'input type for inserting object relation for remote table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'constraint',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: { kind: 'ENUM', name: 'user_constraint', ofType: null },
              },
              description: null,
            },
            {
              name: 'update_columns',
              defaultValue: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'user_update_column',
                      ofType: null,
                    },
                  },
                },
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_on_conflict',
          enumValues: null,
          description: 'on conflict condition type for table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
            {
              name: 'postsByUserId_aggregate',
              defaultValue: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'post_aggregate_order_by',
                ofType: null,
              },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_order_by',
          enumValues: null,
          description: 'ordering options when selecting data from "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_prepend_input',
          enumValues: null,
          description:
            'prepend existing jsonb value of filtered columns with new jsonb value',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'user_select_column',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'address',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              description: 'column name',
            },
          ],
          description: 'select columns of table "user"',
          fields: null,
        },
        {
          inputFields: [
            {
              name: 'address',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'jsonb', ofType: null },
              description: null,
            },
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
            {
              name: 'name',
              defaultValue: null,
              type: { kind: 'SCALAR', name: 'String', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_set_input',
          enumValues: null,
          description: 'input type for updating data in table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_stddev_fields',
          enumValues: null,
          description: 'aggregate stddev on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_stddev_order_by',
          enumValues: null,
          description: 'order by stddev() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_stddev_pop_fields',
          enumValues: null,
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_stddev_pop_order_by',
          enumValues: null,
          description: 'order by stddev_pop() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_stddev_samp_fields',
          enumValues: null,
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_stddev_samp_order_by',
          enumValues: null,
          description: 'order by stddev_samp() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_sum_fields',
          enumValues: null,
          description: 'aggregate sum on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'numeric', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_sum_order_by',
          enumValues: null,
          description: 'order by sum() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'ENUM',
          possibleTypes: null,
          interfaces: null,
          name: 'user_update_column',
          enumValues: [
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'address',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              description: 'column name',
            },
            {
              isDeprecated: false,
              deprecationReason: null,
              name: 'name',
              description: 'column name',
            },
          ],
          description: 'update columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_var_pop_fields',
          enumValues: null,
          description: 'aggregate var_pop on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_var_pop_order_by',
          enumValues: null,
          description: 'order by var_pop() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_var_samp_fields',
          enumValues: null,
          description: 'aggregate var_samp on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_var_samp_order_by',
          enumValues: null,
          description: 'order by var_samp() on columns of table "user"',
          fields: null,
        },
        {
          inputFields: null,
          kind: 'OBJECT',
          possibleTypes: null,
          interfaces: [],
          name: 'user_variance_fields',
          enumValues: null,
          description: 'aggregate variance on columns',
          fields: [
            {
              args: [],
              isDeprecated: false,
              deprecationReason: null,
              name: 'id',
              type: { kind: 'SCALAR', name: 'Float', ofType: null },
              description: null,
            },
          ],
        },
        {
          inputFields: [
            {
              name: 'id',
              defaultValue: null,
              type: { kind: 'ENUM', name: 'order_by', ofType: null },
              description: null,
            },
          ],
          kind: 'INPUT_OBJECT',
          possibleTypes: null,
          interfaces: null,
          name: 'user_variance_order_by',
          enumValues: null,
          description: 'order by variance() on columns of table "user"',
          fields: null,
        },
      ],
      mutationType: { name: 'mutation_root' },
    },
  },
};

const getUnderlyingType = t => {
  let _type = t;
  while (isWrappingType(_type)) {
    _type = _type.ofType;
  }
  return _type;
};

export const getSchemaTree = (relationship, fields) => {
  const { remoteField } = relationship;
  const schemaTree = [];

  const isArgChecked = (
    arg,
    fieldNesting,
    argNesting,
    parentField,
    parentArg
  ) => {
    if (parentField.arguments) {
      if (
        parentField.arguments.find(
          a =>
            a.name === arg.name &&
            a.argNesting === argNesting &&
            a.parentArg === parentArg
        )
      ) {
        return true;
      }
    }
    return false;
  };

  const handleArg = (arg, nesting, argNesting, parentField, parentArg) => {
    const isChecked = isArgChecked(
      arg,
      nesting,
      argNesting,
      parentField,
      parentArg
    );
    schemaTree.push({
      name: arg.name,
      nesting,
      argNesting,
      isChecked,
      isScalar:
        isScalarType(getUnderlyingType(arg.type)) ||
        isEnumType(getUnderlyingType(arg.type)),
      isScalarList:
        isListType(arg.type) &&
        (isScalarType(arg.type.ofType) || isEnumType(arg.type.ofType)),
      isNonNullableScalar:
        isNonNullType(arg.type) &&
        (isScalarType(arg.type.ofType) || isEnumType(arg.type.ofType)),
      isArg: true,
      parentFieldName: parentField.name,
      parentFieldNesting: parentField.nesting,
      parentArg,
    });
    if (isChecked) {
      const handleWrappingTypeArg = __fieldtype => {
        const currentFieldType = getUnderlyingType(__fieldtype);
        if (currentFieldType._fields) {
          Object.values(currentFieldType._fields).forEach(fa =>
            handleArg(
              fa,
              nesting,
              argNesting + 1,
              parentField,
              `${parentArg}.${arg.name}`
            )
          );
        }
      };
      const handleInputObjectTypeArg = __fieldtype => {
        if (__fieldtype._fields) {
          Object.values(__fieldtype._fields).forEach(fa =>
            handleArg(
              fa,
              nesting,
              argNesting + 1,
              parentField,
              `${parentArg}.${arg.name}`
            )
          );
        }
      };
      if (isWrappingType(arg.type)) {
        handleWrappingTypeArg(arg.type);
      } else if (isInputObjectType(arg.type) || isInterfaceType(arg.type)) {
        handleInputObjectTypeArg(arg.type);
      }
    }
  };

  const isFieldChecked = (field, nesting) => {
    if (
      remoteField.find(rf => field.name === rf.name && nesting === rf.nesting)
    ) {
      return true;
    }
    return false;
  };
  const handleField = (field, nesting) => {
    const isChecked = isFieldChecked(field, nesting);
    schemaTree.push({
      name: field.name,
      nesting,
      isChecked,
    });
    if (isChecked) {
      const currentSelectedField = remoteField.find(
        rf => field.name === rf.name && nesting === rf.nesting
      );
      field.args.forEach(fa => {
        handleArg(fa, nesting + 1, 0, currentSelectedField, '');
      });

      const handleScalarTypeField = __fieldtype => {
        handleField(__fieldtype);
      };

      const handleObjectTypeField = __fieldtype => {
        Object.values(__fieldtype._fields).forEach(f =>
          handleField(f, nesting + 1)
        );
      };

      const handleListTypeField = __fieldtype => {
        if (isListType(__fieldtype)) {
          handleListTypeField(__fieldtype.ofType);
        } else if (isObjectType(__fieldtype) || isInterfaceType(__fieldtype)) {
          handleObjectTypeField(__fieldtype);
        } else {
          handleScalarTypeField(__fieldtype);
        }
      };

      if (isListType(field.type) || isNonNullType(field.type)) {
        handleListTypeField(field.type.ofType);
      } else if (isObjectType(field.type) || isInterfaceType(field.type)) {
        handleObjectTypeField(field.type);
      } else {
        handleScalarTypeField(field.type);
      }
    }
  };

  fields.forEach(f => handleField(f, 0));

  return schemaTree;
};

export const transformRemoteRelState = (remoteRel, table) => {
  const payload = {};
  const { remoteField, name, remoteSchema } = remoteRel;
  payload.name = name;
  payload.remote_schema = remoteSchema;
  payload.table = table;
  payload.hasura_fields = [];
  const getArgs = (field, argNesting, parentArg, _argObj) => {
    const argObj = { ..._argObj };
    field.arguments.forEach(a => {
      if (a.argNesting === argNesting && parentArg === a.parentArg) {
        if (a.column) {
          argObj[a.name] = `$${a.column}`;
          payload.hasura_fields.push(a.column);
        } else {
          argObj[a.name] = getArgs(
            field,
            argNesting + 1,
            `${parentArg}.${a.name}`,
            {}
          );
        }
      }
    });
    return argObj;
  };

  const getRemoteFieldObj = nesting => {
    const _rf = remoteField.find(rf => rf.nesting === nesting);
    if (!_rf) {
      return undefined;
    }
    return {
      [_rf.name]: {
        arguments: getArgs(_rf, 0, '', {}),
        field: getRemoteFieldObj(nesting + 1),
      },
    };
  };

  payload.remote_field = getRemoteFieldObj(0);
  return payload;
};

export const parseRemoteRelationship = remoteRel => {
  let _remoteField = { ...remoteRel.remote_field };
  const remoteFields = [];
  let nesting = 0;

  const getArgs = field => {
    const argsList = [];
    const serialiseArgs = (args, argNesting, parentArg) => {
      Object.keys(args).forEach((a, i) => {
        const argObj = {
          name: a,
          parentArg,
          argNesting,
        };
        const argValue = Object.values(args)[i];
        if (typeof argValue === 'string') {
          argObj.column = argValue.substr(1);
        }
        argsList.push(argObj);
        if (typeof argValue === 'object') {
          serialiseArgs(argValue, argNesting + 1, `${parentArg}.${a}`);
        }
      });
    };
    serialiseArgs(field.arguments, 0, '');
    return argsList;
  };

  while (_remoteField && Object.keys(_remoteField).length > 0) {
    remoteFields.push({
      name: Object.keys(_remoteField)[0],
      nesting,
      arguments: getArgs(Object.values(_remoteField)[0]),
    });
    _remoteField = Object.values(_remoteField)[0].field;
    nesting++;
  }
  return {
    name: remoteRel.name,
    remoteSchema: remoteRel.remote_schema,
    remoteField: remoteFields,
  };
};
