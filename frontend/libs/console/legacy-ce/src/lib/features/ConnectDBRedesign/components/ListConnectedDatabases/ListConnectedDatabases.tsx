import React, { useState } from 'react';
import { BiTimer } from 'react-icons/bi';
import { FaEdit, FaTrash, FaUndo } from 'react-icons/fa';
import {
  useDestructiveAlert,
  Button,
  CardedTable,
  IndicatorCard,
  hasuraToast,
} from '@hasura/shared/ui';
import { dataRoutes, getProjectId, isCloudConsole } from '@hasura/shared/utils';
import {
  useInconsistentMetadata,
  useMetadata,
  inconsistentSourcesSelector,
  useDropSource,
} from '@hasura/metadata/api';
import { Source } from '@hasura/shared/types';
import { useDatabaseLatencyCheck } from '../../hooks/useDatabaseLatencyCheck';
import { useReloadSource } from '../../hooks/useReloadSource';
import { useUpdateProjectRegion } from '../../hooks/useUpdateProjectRegion';
import { Latency } from '../../types';
import { AccelerateProject, Details, LatencyBadge } from './parts';
import { useNavigate } from 'react-router';
import {
  NotImplementedError,
  useDatabaseVersion,
} from '@hasura/metadata/data-source';
import { Skeleton } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';

type DatabaseItem = {
  dataSourceName: Source['name'];
  driver: Source['kind'];
};

export const ListConnectedDatabases = (props?: { className?: string }) => {
  const navigate = useNavigate();
  const { envVars } = useAppContext();
  const [showAccelerateProjectSection, setShowAccelerateProjectSection] =
    useState(false);

  const {
    data: databaseList,
    isLoading,
    isFetching,
    refetch: refetchMetadata,
  } = useMetadata((m) =>
    m.metadata.sources.map((source) => ({
      dataSourceName: source.name,
      driver: source.kind,
    })),
  );

  const {
    data: { latencies, rowId } = {},
    refetch,
    isLoading: databaseCheckLoading,
    isSuccess: isDatabaseCheckSuccess,
    isError: isDatabaseCheckError,
  } = useDatabaseLatencyCheck({
    enabled: false,
  });

  React.useEffect(() => {
    if (isDatabaseCheckSuccess) {
      const result = (latencies as any as Latency[]) ?? [];
      const isAnyLatencyHigh = result.find(
        (latency) => latency.avgLatency > 200,
      );
      setShowAccelerateProjectSection(!!isAnyLatencyHigh);
    }
  }, [isDatabaseCheckSuccess, latencies]);

  React.useEffect(() => {
    if (isDatabaseCheckError) {
      hasuraToast({
        type: 'error',
        title: 'Could not fetch latency data!',
        message: 'Something went wrong',
      });
      setShowAccelerateProjectSection(false);
    }
  }, [isDatabaseCheckError]);

  const [activeRow, setActiveRow] = useState<number>();

  const { reloadSource, isPending: isSourceReloading } = useReloadSource();

  const { dropSource, isPending: isSourceRemovalInProgress } = useDropSource({
    onSuccess: () => {
      refetchMetadata();
    },
  });

  const {
    data: inconsistentSources,
    isLoading: isInconsistentFetchCallLoading,
  } = useInconsistentMetadata(inconsistentSourcesSelector);

  const {
    data: databaseVersions,
    isLoading: isDatabaseVersionLoading,
    error: databaseVersionError,
  } = useDatabaseVersion(
    (databaseList ?? []).map((d) => d.dataSourceName),
    !isFetching,
  );

  const isCurrentRow = React.useCallback(
    (rowIndex: number) => rowIndex === activeRow,
    [activeRow],
  );

  const columns = ['database', 'driver', '', ''];

  const handleEdit = React.useCallback((databaseItem: DatabaseItem) => {
    navigate(
      dataRoutes.editDatabase({
        name: databaseItem.dataSourceName,
        kind: databaseItem.driver,
      }),
    );
  }, []);

  const { destructivePrompt } = useDestructiveAlert();

  const handleRemove = React.useCallback(
    (databaseItem: DatabaseItem) => {
      destructivePrompt({
        resourceName: databaseItem.dataSourceName,
        resourceType: 'Data Source',
        destroyTerm: 'remove',
        appendTerm:
          'Any metadata dependent objects (relationships, permissions etc.) from other sources will also be dropped as a result.',
        onConfirm: () => {
          return new Promise((resolve) => {
            return dropSource(
              {
                source: {
                  kind: databaseItem.driver,
                  name: databaseItem.dataSourceName,
                },
              },
              {
                onSuccess: () => resolve(true),
                onError: () => resolve(false),
              },
            );
          });
        },
      });
    },
    [destructivePrompt, dropSource],
  );

  const rowData = React.useMemo(
    () =>
      (databaseList ?? []).map((databaseItem, index) => [
        <div key={`name-${databaseItem.dataSourceName}`}>
          {databaseItem.dataSourceName}
        </div>,
        databaseItem.driver,
        isDatabaseVersionLoading || isInconsistentFetchCallLoading ? (
          <Skeleton
            key={`version-${databaseItem.dataSourceName}`}
            height="20px"
            width="200px"
          />
        ) : (
          <Details
            key={`version-${databaseItem.dataSourceName}`}
            inconsistentSources={inconsistentSources ?? []}
            isSupported={
              !(
                databaseVersionError &&
                databaseVersionError instanceof NotImplementedError
              )
            }
            details={{
              version:
                (databaseVersions ?? []).find(
                  (entry) =>
                    entry.dataSourceName === databaseItem.dataSourceName,
                )?.version ?? '',
            }}
            dataSourceName={databaseItem.dataSourceName}
          />
        ),
        <div
          key={`latency-${databaseItem.dataSourceName}`}
          className="flex justify-center"
        >
          <LatencyBadge
            latencies={latencies ?? []}
            dataSourceName={databaseItem.dataSourceName}
          />
        </div>,
        <div
          key={`actions-${databaseItem.dataSourceName}`}
          className="flex gap-4 justify-end px-4"
          onClick={(e) => {
            setActiveRow(index);
          }}
        >
          <Button
            mode="default"
            leftIcon={FaUndo}
            size="1"
            onClick={() => reloadSource(databaseItem.dataSourceName)}
            loading={isSourceReloading && isCurrentRow(index)}
            loadingText="Reloading"
          >
            Reload
          </Button>
          <Button
            mode="primary"
            leftIcon={FaEdit}
            size="1"
            onClick={() => handleEdit(databaseItem)}
          >
            Edit
          </Button>
          <Button
            leftIcon={FaTrash}
            mode="destructive"
            size="1"
            onClick={() => handleRemove(databaseItem)}
            loading={isSourceRemovalInProgress && isCurrentRow(index)}
            loadingText="Deleting"
          >
            Remove
          </Button>
        </div>,
      ]),
    [
      databaseList,
      databaseVersions,
      handleEdit,
      handleRemove,
      inconsistentSources,
      isCurrentRow,
      isDatabaseVersionLoading,
      isInconsistentFetchCallLoading,
      isSourceReloading,
      isSourceRemovalInProgress,
      latencies,
      reloadSource,
    ],
  );

  const {
    mutate: updateProjectRegionForRowId,
    // isLoading: isUpdatingProjectRegion,
  } = useUpdateProjectRegion();

  const openUpdateProjectRegionPage = React.useCallback(
    (_rowId?: string) => {
      if (!_rowId) {
        hasuraToast({
          type: 'error',
          title: 'Could not fetch row Id to update!',
          message: 'Something went wrong',
        });
        return;
      }

      // update project region for the row Id
      updateProjectRegionForRowId(_rowId);

      // redirect to the cloud "change region for project page"

      const projectId = getProjectId(envVars);
      if (!projectId) {
        return;
      }
      const cloudDetailsPage = `${window.location.protocol}//${window.location.host}/project/${projectId}/details?open_update_region_drawer=true`;

      window.open(cloudDetailsPage, '_blank');
    },
    [updateProjectRegionForRowId],
  );

  if (isLoading) return <>Loading...</>;

  return (
    <div className={props?.className}>
      {rowData.length ? (
        <CardedTable columns={[...columns, null]} data={rowData} />
      ) : (
        <IndicatorCard headline="No databases connected">
          You don&apos;t have any data sources connected, please connect one to
          continue.
        </IndicatorCard>
      )}

      {showAccelerateProjectSection ? (
        <AccelerateProject
          isLoading={databaseCheckLoading}
          onReCheckClick={() => {
            refetch();
          }}
          onUpdateRegionClick={() => {
            openUpdateProjectRegionPage(rowId);
          }}
        />
      ) : (
        isCloudConsole(envVars) && (
          <Button
            onClick={() => {
              refetch();
            }}
            leftIcon={BiTimer}
            loading={databaseCheckLoading}
            loadingText="Measuring Latencies"
          >
            Check latency
          </Button>
        )
      )}
    </div>
  );
};
