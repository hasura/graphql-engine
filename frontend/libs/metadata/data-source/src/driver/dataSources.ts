import { DataSourceNetworkArgs, DriverInfo, ReleaseType } from './types';
import { NativeDriver, SupportedDriver } from '@hasura/shared/types';
import { Database } from './types/database';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { cockroach } from './cockroach';
import { gdc } from './gdc';
import { mssql } from './mssql';
import { postgres } from './postgres';
import { alloy } from './alloydb';
import { getAllSourceKinds as _getAllSourceKinds } from './common/getAllSourceKinds';
import {
  isKnownEnterpriseSourceKind,
  isNativeDriver,
} from '@hasura/metadata/helpers';
import pickBy from 'lodash/pickBy';
import { z } from 'zod';
import { transformSchemaToZodObject } from '@hasura/shared/ui';

type SupportedDatabase = NativeDriver | 'gdc';

const drivers: Record<SupportedDatabase, Database> = {
  postgres,
  bigquery,
  citus,
  mssql,
  gdc,
  cockroach,
  alloy,
};

export const getDatabaseMethods = (driver: string) => {
  if (driver === 'pg') return drivers.postgres;

  if (isNativeDriver(driver)) return drivers[driver];

  return drivers.gdc;
};

export const getDatabaseKind = (kind: string): SupportedDatabase => {
  if (isNativeDriver(kind)) {
    return kind as SupportedDatabase;
  }

  return 'gdc';
};

export async function getAllSourceKinds(
  args: DataSourceNetworkArgs,
): Promise<DriverInfo[]> {
  const serverSupportedDrivers = await _getAllSourceKinds(args);
  const allSupportedDrivers = serverSupportedDrivers
    // NOTE: AlloyDB is added here and not returned by the server because it's not a new data source (it's Postgres)
    .concat([
      {
        builtin: true,
        kind: 'alloy',
        display_name: 'AlloyDB',
        available: true,
      },
    ])
    .sort((a, b) => (a.kind > b.kind ? 1 : -1));

  const allDrivers = allSupportedDrivers.map(async (driver) => {
    const getDriverInfo = getDatabaseMethods(driver.kind).introspection
      ?.getDriverInfo;
    if (!getDriverInfo) {
      return {
        name: driver.kind,
        displayName: driver.display_name,
        release: (driver.release_name as ReleaseType) ?? 'GA',
        native: driver.builtin,
        available: true,
        enterprise: false,
      };
    }

    const driverInfo = await getDriverInfo();

    return {
      name: driverInfo.name,
      displayName: driverInfo.displayName || driver.display_name,
      release: driverInfo.release ?? driver.release_name ?? 'GA',
      native: driverInfo.native ?? driver.builtin,
      available: driverInfo.available,
      enterprise: isKnownEnterpriseSourceKind(driver.kind),
    };
  });

  return Promise.all(allDrivers);
}

export const getConnectDatabaseFormSchema = async (
  args: { driver: SupportedDriver } & DataSourceNetworkArgs,
) => {
  const databaseMethods = getDatabaseMethods(args.driver);
  if (!databaseMethods.introspection?.getDatabaseConfiguration) {
    return null;
  }

  const schema =
    await databaseMethods.introspection?.getDatabaseConfiguration(args);

  if (!schema) return;

  return z.object({
    driver: z.literal(args.driver),
    name: z.string().min(1, 'Name is a required field!'),
    replace_configuration: z.preprocess((x) => {
      if (!x) return false;
      return true;
    }, z.boolean()),
    configuration: transformSchemaToZodObject(
      schema.configSchema,
      schema.otherSchemas,
    ),
    customization: z
      .object({
        root_fields: z
          .object({
            namespace: z.string().optional(),
            prefix: z.string().optional(),
            suffix: z.string().optional(),
          })
          .transform((value) => pickBy(value, (d) => d !== ''))
          .optional(),
        type_names: z
          .object({
            prefix: z.string().optional(),
            suffix: z.string().optional(),
          })
          .transform((value) => pickBy(value, (d) => d !== ''))
          .optional(),
      })
      .optional(),
  });
};
