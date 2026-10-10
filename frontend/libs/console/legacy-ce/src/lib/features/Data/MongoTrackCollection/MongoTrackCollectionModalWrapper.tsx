import { DisplayToastErrorMessage, hasuraToast } from '@hasura/shared/ui';
import {
  useMetadataMigration,
  useMetadata,
  useTrackTables,
  getTrackTablesArgs,
} from '@hasura/metadata/api';
import { LogicalModel } from '@hasura/shared/types';
import { COLLECTION_TRACK_TRACK_ERROR } from '../LogicalModels/constants';
import { getTrackLogicalModelPayload } from '../hooks/useTrackLogicalModel';
import { MongoTrackCollectionModal, Schema } from './MongoTrackCollectionModal';
import { areTablesEqual, MetadataSelectors } from '@hasura/metadata/helpers';

type MongoTrackCollectionModalProps = {
  dataSourceName: string;
  collectionName: string;
  isVisible: boolean;
  onClose: () => void;
};

export const MongoTrackCollectionModalWrapper = ({
  dataSourceName,
  collectionName,
  onClose,
  isVisible,
}: MongoTrackCollectionModalProps) => {
  const { data: meta } = useMetadata();
  const sources = meta?.metadata.sources ?? [];
  const source = MetadataSelectors.findSource(dataSourceName)(meta);
  const logicalModels = source?.logical_models ?? [];
  const metadataTable = source?.tables.find((t) =>
    areTablesEqual(t.table, [collectionName]),
  );
  const configuration = metadataTable?.configuration;
  const driver = source?.kind;

  const { trackTables } = useTrackTables();

  const { mutate: migrateMetadata, isPending: isTracking } =
    useMetadataMigration({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Collection tracked successfully',
        });
      },
      onError: (err) => {
        hasuraToast({
          type: 'error',
          title: COLLECTION_TRACK_TRACK_ERROR,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const onSubmit = async (data: Schema, logicalModels: LogicalModel[]) => {
    if (data.logicalModelForm === 'json-validation-schema') {
      trackTables({
        source: dataSourceName,
        tables: [
          {
            table: [collectionName],
            configuration: {
              ...(data.custom_name ? { custom_name: data.custom_name } : {}),
              ...(data.custom_root_fields
                ? { custom_root_fields: data.custom_root_fields }
                : {}),
            },
          },
        ],
      });
    } else if (data.logicalModelForm === 'sample-documents') {
      // create logical model and track collection
      const trackLogicalModelsPayload = logicalModels.map((logicalModel) =>
        getTrackLogicalModelPayload({
          data: {
            dataSourceName: dataSourceName,
            name: logicalModel.name,
            fields: logicalModel.fields,
          },
          sources,
        }),
      );

      const trackTablesPayload = getTrackTablesArgs(
        {
          tables: [
            {
              source: dataSourceName,
              table: [collectionName],
              configuration: {
                ...(data.custom_name ? { custom_name: data.custom_name } : {}),
                ...(data.custom_root_fields
                  ? { custom_root_fields: data.custom_root_fields }
                  : {}),
                logical_model: logicalModels[0].name,
              },
            },
          ],
        },
        driver ?? 'mongodb',
      );

      migrateMetadata({
        query: {
          resource_version: meta?.resource_version,
          type: 'bulk',
          args: [
            ...trackLogicalModelsPayload.flat().reverse(),
            trackTablesPayload,
          ],
        },
      });
    }

    if (data.logicalModelForm === 'logical-models') {
      // track collection with selected logical model
      const trackTablesPayload = getTrackTablesArgs(
        {
          tables: [
            {
              source: dataSourceName,
              table: [collectionName],
              configuration: {
                ...(data.custom_name ? { custom_name: data.custom_name } : {}),
                ...(data.custom_root_fields
                  ? { custom_root_fields: data.custom_root_fields }
                  : {}),
                logical_model: logicalModels[0].name,
              },
            },
          ],
        },
        driver!,
      );

      migrateMetadata({
        query: {
          resource_version: meta?.resource_version,
          type: 'bulk',
          args: [trackTablesPayload],
        },
      });
    }
  };

  return (
    <MongoTrackCollectionModal
      dataSourceName={dataSourceName}
      collectionName={collectionName}
      collectionConfiguration={configuration}
      isVisible={isVisible}
      onClose={onClose}
      logicalModels={logicalModels || []}
      onSubmit={onSubmit}
      isLoading={isTracking}
    />
  );
};
