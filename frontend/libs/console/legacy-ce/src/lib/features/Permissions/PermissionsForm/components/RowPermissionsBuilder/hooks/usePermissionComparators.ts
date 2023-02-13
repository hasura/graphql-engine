import { useIntrospectSchema } from '.';
import { comparatorsFromSchema } from '../components/utils/comparatorsFromSchema';

export function usePermissionComparators() {
  const { data: schema } = useIntrospectSchema();
  return schema ? comparatorsFromSchema(schema) : {};
}
