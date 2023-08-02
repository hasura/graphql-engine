import {
  NativeQuery as NativeQueryMetadataType,
  NativeQueryArgument,
} from '../../../hasura-metadata-types';

// metadata has these as records, but it's easier to do an array shape for the form
export type NativeQueryArgumentNormalized = {
  name: string;
} & NativeQueryArgument;

// slightly adapted type from metadata to use in form
export type NativeQueryForm = Omit<NativeQueryMetadataType, 'arguments'> & {
  arguments: NativeQueryArgumentNormalized[];
  source: string;
};
