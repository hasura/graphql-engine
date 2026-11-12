import { useMemo } from 'react';
import { useIntrospectSchema } from '@hasura/metadata/api';
import { comparatorsFromSchema } from '../components/utils/comparatorsFromSchema';

export function usePermissionComparators() {
  const { data: schema } = useIntrospectSchema();
  // walks every type of the schema, so only once per schema
  return useMemo(() => (schema ? comparatorsFromSchema(schema) : {}), [schema]);
}
