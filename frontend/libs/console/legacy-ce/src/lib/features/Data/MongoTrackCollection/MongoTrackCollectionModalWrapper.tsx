import { hasuraToast } from '../../../new-components/Toasts';
import { useMetadataMigration } from '../../MetadataAPI';
import {
  MetadataSelectors,
  MetadataUtils,
  useMetadata,
} from '../../hasura-metadata-api';
import { LogicalModel } from '../../hasura-metadata-types';
import { COLLECTION_TRACK_TRACK_ERROR } from '../LogicalModels/constants';
import { DisplayToastErrorMessage } from '../components/DisplayErrorMessage';
import { transformErrorResponse } from '../errorUtils';
import { useTrackLogicalModel } from '../hooks/useTrackLogicalModel';
import { useTrackTables } from '../hooks/useTrackTables';
import { MongoTrackCollectionModal, Schema } from './MongoTrackCollectionModal';

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
  const { data: logicalModels } = useMetadata(
    m => MetadataUtils.findMetadataSource(dataSourceName, m)?.logical_models
  );

  const { data: configuration } = useMetadata(
    m =>
      MetadataUtils.findMetadataTable(dataSourceName, collectionName, m)
        ?.configuration
  );

  const { data: { driver, sources = [], resource_version } = {} } = useMetadata(
    m => ({
      driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
      sources: m.metadata.sources,
      resource_version: m.resource_version,
    })
  );

  const { getTrackLogicalModelPayload } = useTrackLogicalModel();
  const { getTrackTablesPayload } = useTrackTables({
    dataSourceName,
  });

  const { mutate, isLoading: isTracking } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Collection tracked successfully',
      });
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: COLLECTION_TRACK_TRACK_ERROR,
        children: <DisplayToastErrorMessage message={err.message} />,
      });
    },
    errorTransform: transformErrorResponse,
  });

  const onSubmit = async (data: Schema, logicalModels: LogicalModel[]) => {
    if (data.logicalModelForm === 'json-validation-schema') {
      const trackTablesPayload = getTrackTablesPayload({
        driver,
        dataSourceName,
        tables: [
          {
            id: '',
            type: 'collection',
            name: collectionName,
            table: [collectionName],
            is_tracked: false,
            configuration: {
              ...(data.custom_name ? { custom_name: data.custom_name } : {}),
              ...(data.custom_root_fields
                ? { custom_root_fields: data.custom_root_fields }
                : {}),
            },
          },
        ],
      });

      mutate({
        query: {
          resource_version,
          type: 'bulk',
          args: [trackTablesPayload],
        },
      });
    }

    if (data.logicalModelForm === 'sample-documents') {
      // create logical model and track collection
      const trackLogicalModelsPayload = logicalModels.map(logicalModel =>
        getTrackLogicalModelPayload({
          data: {
            dataSourceName: dataSourceName,
            name: logicalModel.name,
            fields: logicalModel.fields,
          },
          sources,
        })
      );

      const trackTablesPayload = getTrackTablesPayload({
        driver,
        dataSourceName,
        tables: [
          {
            id: '',
            type: 'collection',
            name: collectionName,
            table: [collectionName],
            is_tracked: false,
            configuration: {
              ...(data.custom_name ? { custom_name: data.custom_name } : {}),
              ...(data.custom_root_fields
                ? { custom_root_fields: data.custom_root_fields }
                : {}),
              logical_model: logicalModels[0].name,
            },
          },
        ],
      });

      mutate({
        query: {
          resource_version,
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
      const trackTablesPayload = getTrackTablesPayload({
        driver,
        dataSourceName,
        tables: [
          {
            id: '',
            type: 'collection',
            name: collectionName,
            table: [collectionName],
            is_tracked: false,
            configuration: {
              ...(data.custom_name ? { custom_name: data.custom_name } : {}),
              ...(data.custom_root_fields
                ? { custom_root_fields: data.custom_root_fields }
                : {}),
              logical_model: logicalModels[0].name,
            },
          },
        ],
      });

      mutate({
        query: {
          resource_version,
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
