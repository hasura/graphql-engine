import { sendTelemetryEvent } from '../../telemetry';
import {
  DataSourceNetworkArgs,
  getDatabaseMethods,
} from '@hasura/metadata/data-source';
import type { SupportedDriver } from '@hasura/shared/types';
import { hashString } from '@hasura/shared/utils';

// returns the correct indefinite article based on the first character of the input string
export const indefiniteArticle = (word: string): string => {
  const vowels = ['a', 'e', 'i', 'o', 'u'];
  return vowels.includes(word.charAt(0)) ? 'an' : 'a';
};

export const getDriverNameFromUrlParams = (): string | undefined => {
  const urlParams = new URLSearchParams(window.location.search);

  const driver = urlParams.get('driver');

  return driver ?? undefined;
};

export const sendConnectDatabaseTelemetryEvent = async ({
  dataSourceName,
  driver,
  ...rest
}: {
  dataSourceName: string;
  driver: SupportedDriver;
} & DataSourceNetworkArgs) => {
  const databaseMethods = getDatabaseMethods(driver);
  if (databaseMethods.introspection.getTrackableTables) {
    const tables = await databaseMethods.introspection.getTrackableTables({
      dataSourceName,
      configuration: null,
      ...rest,
    });
    const entities = tables.map((table) => table.name);
    // ensure a consistent hash for the same set of tables. ie. we would like to have ["article", "author"] and ["author", "article"] to result in the same hash
    entities.sort();
    const entity_count = entities.length;
    const entity_hash = entity_count
      ? await hashString(entities.toString())
      : '00000000000000000000000000000000';
    sendTelemetryEvent({
      type: 'CONNECT_DB',
      data: {
        db_kind: driver,
        entity_count,
        entity_hash,
      },
    });
    return;
  }
  // When introspection is not supported, at least send db_kind so that we know connected DBs
  sendTelemetryEvent({
    type: 'CONNECT_DB',
    data: {
      db_kind: driver,
    },
  });
};
