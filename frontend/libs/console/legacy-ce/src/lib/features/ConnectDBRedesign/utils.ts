import { AxiosInstance } from 'axios';
import { sendTelemetryEvent } from '../../telemetry';
import { Driver } from '../../dataSources';
import { DataSource, Feature } from '../DataSource';
import { hashString } from '../../components/Common/utils/jsUtils';

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

export const transformErrorResponse = (error: unknown) => {
  const err = error as Record<string, any>;

  let message = '';

  if ('internal' in err) message = JSON.stringify(err?.internal, null, '\t');
  else message = err.error;

  return {
    name: `Error code: ${err.code}`,
    message,
  };
};

export const sendConnectDatabaseTelemetryEvent = async ({
  httpClient,
  dataSourceName,
  driver,
}: {
  dataSourceName: string;
  driver: Driver;
  httpClient: AxiosInstance;
}) => {
  const tables = await DataSource(httpClient).introspectTables({
    dataSourceName,
  });
  if (tables !== Feature.NotImplemented) {
    const entities = tables.map(table => table.name);
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
