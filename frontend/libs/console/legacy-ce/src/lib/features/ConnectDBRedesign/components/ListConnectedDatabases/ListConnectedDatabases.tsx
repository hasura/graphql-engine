import to from 'await-to-js';
import React, { useState } from 'react';
import { BiTimer } from 'react-icons/bi';
import { FaEdit, FaTrash, FaUndo } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import globals from '../../../../Globals';
import _push from '../../../../components/Services/Data/push';
import { exportMetadata } from '../../../../metadata/actions';
import { useDestructiveAlert } from '../../../../new-components/Alert';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useAppDispatch } from '../../../../storeHooks';
import { getProjectId, isCloudConsole } from '../../../../utils/cloudConsole';

import { useMetadata } from '../../../hasura-metadata-api';
import { Source } from '../../../hasura-metadata-types';
import { useDatabaseLatencyCheck } from '../../hooks/useDatabaseLatencyCheck';
import { useDatabaseVersion } from '../../hooks/useDatabaseVersion';
import { useDropSource } from '../../hooks/useDropSource';
import { useInconsistentSources } from '../../hooks/useInconsistentSources';
import { useReloadSource } from '../../hooks/useReloadSource';
import { useUpdateProjectRegion } from '../../hooks/useUpdateProjectRegion';
import { Latency } from '../../types';
import { AccelerateProject, Details, LatencyBadge } from './parts';

type DatabaseItem = {
  dataSourceName: Source['name'];
  driver: Source['kind'];
};

export const ListConnectedDatabases = (props?: { className?: string }) => {
  const [showAccelerateProjectSection, setShowAccelerateProjectSection] =
    useState(false);

  const {
    data: { latencies, rowId } = {},
    refetch,
    isLoading: databaseCheckLoading,
  } = useDatabaseLatencyCheck({
    enabled: false,
    onSuccess: data => {
      const result = (data as any).latencies as Latency[];
      const isAnyLatencyHigh = result.find(latency => latency.avgLatency > 200);
      setShowAccelerateProjectSection(!!isAnyLatencyHigh);
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: 'Could not fetch latency data!',
        message: 'Something went wrong',
      });
      setShowAccelerateProjectSection(false);
    },
  });

  const dispatch = useAppDispatch();

  const [activeRow, setActiveRow] = useState<number>();

  const { reloadSource, isLoading: isSourceReloading } = useReloadSource();

  const { dropSource, isLoading: isSourceRemovalInProgress } = useDropSource({
    onSuccess: () => {
      dispatch(exportMetadata());
    },
  });

  const {
    data: inconsistentSources,
    isLoading: isInconsistentFetchCallLoading,
  } = useInconsistentSources();

  const {
    data: databaseList,
    isLoading,
    isFetching,
  } = useMetadata(m =>
    m.metadata.sources.map(source => ({
      dataSourceName: source.name,
      driver: source.kind,
    }))
  );

  const { data: databaseVersions, isLoading: isDatabaseVersionLoading } =
    useDatabaseVersion(
      (databaseList ?? []).map(d => d.dataSourceName),
      !isFetching
    );

  const isCurrentRow = React.useCallback(
    (rowIndex: number) => rowIndex === activeRow,
    [activeRow]
  );

  const columns = ['database', 'driver', '', ''];

  const handleEdit = React.useCallback((databaseItem: DatabaseItem) => {
    dispatch(
      _push(
        `/data/v2/manage/database/edit?driver=${databaseItem.driver}&database=${databaseItem.dataSourceName}`
      )
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
        onConfirm: async () => {
          let success = true;

          const [err] = await to(
            dropSource({
              driver: databaseItem.driver,
              dataSourceName: databaseItem.dataSourceName,
            })
          );

          if (err) {
            success = false;
          }

          return success;
        },
      });
    },
    [destructivePrompt, dropSource]
  );

  const rowData = React.useMemo(
    () =>
      (databaseList ?? []).map((databaseItem, index) => [
        <div>{databaseItem.dataSourceName}</div>,
        databaseItem.driver,
        isDatabaseVersionLoading || isInconsistentFetchCallLoading ? (
          <Skeleton />
        ) : (
          <Details
            inconsistentSources={inconsistentSources ?? []}
            details={{
              version:
                (databaseVersions ?? []).find(
                  entry => entry.dataSourceName === databaseItem.dataSourceName
                )?.version ?? '',
            }}
            dataSourceName={databaseItem.dataSourceName}
          />
        ),
        <div className="flex justify-center">
          <LatencyBadge
            latencies={latencies ?? []}
            dataSourceName={databaseItem.dataSourceName}
          />
        </div>,
        <div
          className="flex gap-4 justify-end px-4"
          onClick={e => {
            setActiveRow(index);
          }}
        >
          <Button
            icon={<FaUndo />}
            size="sm"
            onClick={() => reloadSource(databaseItem.dataSourceName)}
            isLoading={isSourceReloading && isCurrentRow(index)}
            loadingText="Reloading"
          >
            Reload
          </Button>
          <Button
            icon={<FaEdit />}
            size="sm"
            onClick={() => handleEdit(databaseItem)}
          >
            Edit
          </Button>
          <Button
            icon={<FaTrash />}
            mode="destructive"
            size="sm"
            onClick={() => handleRemove(databaseItem)}
            isLoading={isSourceRemovalInProgress && isCurrentRow(index)}
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
    ]
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

      const projectId = getProjectId(globals);
      if (!projectId) {
        return;
      }
      const cloudDetailsPage = `${window.location.protocol}//${window.location.host}/project/${projectId}/details?open_update_region_drawer=true`;

      window.open(cloudDetailsPage, '_blank');
    },
    [updateProjectRegionForRowId]
  );

  if (isLoading) return <>Loading...</>;

  return (
    <div className={props?.className}>
      {rowData.length ? (
        <CardedTable
          columns={[...columns, null]}
          data={rowData}
          showActionCell
        />
      ) : (
        <IndicatorCard headline="No databases connected">
          You don't have any data sources connected, please connect one to
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
        isCloudConsole(globals) && (
          <Button
            onClick={() => {
              refetch();
            }}
            icon={<BiTimer />}
            isLoading={databaseCheckLoading}
            loadingText="Measuring Latencies"
          >
            Check latency
          </Button>
        )
      )}
    </div>
  );
};
