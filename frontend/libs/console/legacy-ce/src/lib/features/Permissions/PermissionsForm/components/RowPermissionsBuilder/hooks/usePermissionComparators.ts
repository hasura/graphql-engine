import { useIntrospectSchema } from '@hasura/metadata/api';
import { comparatorsFromSchema } from '../components/utils/comparatorsFromSchema';

export function usePermissionComparators() {
  const { data: schema } = useIntrospectSchema();
  return schema ? comparatorsFromSchema(schema) : {};
}
