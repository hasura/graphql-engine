import { NativeQueryForm } from './types';
import { NativeQuery as NativeQueryMetadataType } from '../../../hasura-metadata-types';

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
