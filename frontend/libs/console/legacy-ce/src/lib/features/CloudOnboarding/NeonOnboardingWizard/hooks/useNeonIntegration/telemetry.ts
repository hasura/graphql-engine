import { FetchJson, hashString } from '@hasura/shared/utils';
import {
  ConnectDBEvent,
  sendTelemetryEvent,
  trackRuntimeError,
} from '../../../../../telemetry';
import type {
  QualifiedDataSource,
  SupportedDriver,
} from '@hasura/shared/types';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { Endpoints } from '@hasura/shared/context';

const makeConnectDBTelemetryEvent = (
  eventHandler: (event: ConnectDBEvent) => void,
  dbKind: SupportedDriver,
  dbEntities?: string[],
) => {
  // send entity_count only if DB entity data is available
  const connectDBEvent: ConnectDBEvent = {
    type: 'CONNECT_DB',
    data: {
      db_kind: dbKind,
      entity_count: dbEntities ? dbEntities.length : undefined,
      entity_hash: undefined,
    },
  };

  // set entity_hash only if non-zero entities exist
  if (dbEntities?.length) {
    const setEntityHashAndHandleEvent = (hash: string) => {
      connectDBEvent.data.entity_hash = hash;

      eventHandler(connectDBEvent);
    };

    return hashString(dbEntities?.join(',')).then((entityHash) =>
      setEntityHashAndHandleEvent(entityHash),
    );
  } else {
    // set fixed hash value for 0 entities
    connectDBEvent.data.entity_hash = '00000000000000000000000000000000';
    eventHandler(connectDBEvent);
  }
};

export const sendInitialDBStateTelemetry = async (
  endpoints: Endpoints,
  fetchJson: FetchJson,
  source: QualifiedDataSource,
) => {
  try {
    const dbKind = source.kind;
    const dbMethods = getDatabaseMethods(source.kind);
    const dbTableNamesQuery = dbMethods.introspection.getTrackableTables
      ? await dbMethods.introspection.getTrackableTables({
          dataSourceName: source.name,
          endpoints,
          fetchJson,
        })
      : [];

    if (!dbTableNamesQuery.length) {
      makeConnectDBTelemetryEvent(sendTelemetryEvent, dbKind);
      return;
    }

    const tableNames = dbTableNamesQuery.map((t) => t.name);

    return makeConnectDBTelemetryEvent(sendTelemetryEvent, dbKind, tableNames);
  } catch (err) {
    trackRuntimeError(err as Error);
  }
};
