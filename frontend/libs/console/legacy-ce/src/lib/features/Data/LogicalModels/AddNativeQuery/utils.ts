import {
  NativeQueryArgument,
  NativeQuery as NativeQueryMetadataType,
} from '../../../hasura-metadata-types';
import { NativeQueryArgumentNormalized, NativeQueryForm } from './types';

export const transformFormOutputToMetadata = (
  formValues: NativeQueryForm
): NativeQueryMetadataType => {
  //transform array of arguments to the record approach used in metadata
  const queryArgsForMetadata = formValues.arguments.reduce<
    NativeQueryMetadataType['arguments']
  >((result, arg) => {
    const { name, ...rest } = arg;
    return {
      ...result,
      [name]: {
        ...rest,
      },
    };
  }, {});

  // remove source from the object and get the rest...
  const { source, ...rest } = formValues;

  return {
    ...rest,
    arguments: queryArgsForMetadata,
  };

  const { code, returns, root_field_name, comment, type } = formValues;

  return {
    root_field_name,
    comment,
    arguments: queryArgsForMetadata,
    code,
    returns,
    type,
  };
};

export const normalizeArguments = (
  args: Record<string, NativeQueryArgument>
): NativeQueryArgumentNormalized[] =>
  Object.entries(args).map(([name, argument]) => ({
    name,
    ...argument,
  }));
