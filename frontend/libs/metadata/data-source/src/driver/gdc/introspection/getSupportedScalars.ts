import { GetSupportedScalarsProps } from '../../types';
import { getDriverCapabilities } from './getDriverCapabilities';

export async function getSupportedScalars(
  props: GetSupportedScalarsProps,
): Promise<string[]> {
  const capabilities = await getDriverCapabilities(props);
  return Object.keys(capabilities.scalar_types ?? []);
}
