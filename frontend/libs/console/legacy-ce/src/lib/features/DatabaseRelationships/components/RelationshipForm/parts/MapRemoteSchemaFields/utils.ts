import {
  InputArgumentsType,
  InputArgumentValueType,
  RemoteField,
} from '../../../../../hasura-metadata-types';
import { isEmpty } from '../../../../../../components/Common/utils/jsUtils';
import { RemoteSchemaRelationship } from '../../../../types';
import { parseArgValue } from './parts/RemoteSchemaTree/utils';
import { RelationshipFields } from './types';

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
  remoteField: RemoteSchemaRelationship['definition']['remote_field']
): RelationshipFields[] => {
  const remoteFields = remoteField;
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

const matchAll = (re: RegExp, str: string) => {
  let match;
  const matches = [];
  // eslint-disable-next-line no-cond-assign
  while ((match = re.exec(str)) !== null) {
    // ref : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec
    matches.push(match[0]);
  }

  return matches;
};

export const generateLhsFields = (resultSet: Record<string, unknown>) => {
  const regexp = /([$])\w+/g;
  const str = JSON.stringify(resultSet, null, 2);
  const lhs_fieldSet = new Set<string>();

  const results = matchAll(regexp, str);
  results.forEach(i => lhs_fieldSet.add(i.substring(1))); // remove $ symbol from the string to pass as lhs_fields
  return Array.from(lhs_fieldSet);
};
