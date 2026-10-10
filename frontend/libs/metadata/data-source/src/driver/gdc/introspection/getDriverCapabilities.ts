import { GetDriverCapabilitiesArgs } from '../../types';
import { getSourceKindCapabilities } from './getDatabaseConfiguration';

export const getDriverCapabilities = async (
  args: GetDriverCapabilitiesArgs,
) => {
  const result = await getSourceKindCapabilities(args);

  return result.capabilities;
};
