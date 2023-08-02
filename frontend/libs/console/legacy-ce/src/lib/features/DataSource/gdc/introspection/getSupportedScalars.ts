import { GetSupportedScalarsProps } from '../../types';
import { getDriverCapabilities } from './getDriverCapabilities';

export async function getSupportedScalars({
  dataSourceKind,
  httpClient,
}: GetSupportedScalarsProps): Promise<string[]> {
  const capabilities = await getDriverCapabilities(httpClient, dataSourceKind);
  return Object.keys(capabilities.scalar_types ?? []);
}
