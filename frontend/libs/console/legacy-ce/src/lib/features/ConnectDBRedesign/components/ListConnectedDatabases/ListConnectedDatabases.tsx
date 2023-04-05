import { CardedTable } from '../../../../new-components/CardedTable';
import { Button } from '../../../../new-components/Button';
import {
  FaCheck,
  FaEdit,
  FaExclamationTriangle,
  FaMinusCircle,
  FaTrash,
  FaUndo,
  FaRedoAlt,
  FaExternalLinkAlt,
} from 'react-icons/fa';
import { useMetadata } from '../../../MetadataAPI';
import _push from '../../../../components/Services/Data/push';
import { useReloadSource } from '../../hooks/useReloadSource';
import { useDropSource } from '../../hooks/useDropSource';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import Skeleton from 'react-loading-skeleton';
import { useInconsistentSources } from '../../hooks/useInconsistentSources';
import { Details } from './parts/Details';
import { useState } from 'react';
import { useDatabaseVersion } from '../../hooks/useDatabaseVersion';
import { useDatabaseLatencyCheck } from '../../hooks/useDatabaseLatencyCheck';
import { BiTimer } from 'react-icons/bi';
import { hasuraToast } from '../../../../new-components/Toasts';
import { Latency } from '../../types';
import { Badge } from '../../../../new-components/Badge';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { getProjectId, isCloudConsole } from '../../../../utils/cloudConsole';
import globals from '../../../../Globals';
import { useUpdateProjectRegion } from '../../hooks/useUpdateProjectRegion';
import { useAppDispatch } from '../../../../storeHooks';

const LatencyBadge = ({
  latencies,
  dataSourceName,
}: {
  latencies: Latency[];
  dataSourceName: string;
}) => {
  const currentDataSourceLatencyInfo = latencies.find(
    latencyInfo => latencyInfo.dataSourceName === dataSourceName
  );

  if (!currentDataSourceLatencyInfo) return null;

  if (currentDataSourceLatencyInfo.avgLatency < 100)
    return (
      <Badge color="green">
        <FaCheck className="mr-xs" /> Connection
      </Badge>
    );

  if (currentDataSourceLatencyInfo.avgLatency < 200)
    return (
      <Badge color="yellow">
        <FaMinusCircle className="mr-xs" /> Acceptable
      </Badge>
    );

  return (
    <Badge color="red">
      <FaExclamationTriangle className="mr-xs" /> Elevated Latency
    </Badge>
  );
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
      console.log('on success', data);
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
    },
  });

  const {
    mutate: updateProjectRegionForRowId,
    // isLoading: isUpdatingProjectRegion,
  } = useUpdateProjectRegion();

  const dispatch = useAppDispatch();
  const [activeRow, setActiveRow] = useState<number>();
  const { reloadSource, isLoading: isSourceReloading } = useReloadSource();
  const { dropSource, isLoading: isSourceRemovalInProgress } = useDropSource();
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

  const isCurrentRow = (rowIndex: number) => rowIndex === activeRow;

  if (isLoading) return <>Loading...</>;

  const columns = ['database', 'driver', '', ''];

  const rowData = (databaseList ?? []).map((databaseItem, index) => [
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
        console.log('parent event captured');
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
        onClick={() => {
          dispatch(
            _push(
              `/data/v2/manage/database/edit?driver=${databaseItem.driver}&database=${databaseItem.dataSourceName}`
            )
          );
        }}
      >
        Edit
      </Button>
      <Button
        icon={<FaTrash />}
        mode="destructive"
        size="sm"
        onClick={() => {
          dropSource(databaseItem.driver, databaseItem.dataSourceName);
        }}
        isLoading={isSourceRemovalInProgress && isCurrentRow(index)}
        loadingText="Deleting"
      >
        Remove
      </Button>
    </div>,
  ]);

  // console.log(
  //   'loading: ',
  //   databaseCheckLoading,
  //   'result: ',
  //   latencies,
  //   'any error: ',
  //   error
  // );

  const openUpdateProjectRegionPage = (_rowId?: string) => {
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
  };

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
        <div className="mt-xs">
          <IndicatorCard
            status="negative"
            headline="Accelerate your Hasura Project"
          >
            <div className="flex items-center flex-row">
              <span>
                Databases marked with “Elevated Latency” indicate that it took
                us over 200 ms for this Hasura project to communicate with your
                database. These conditions generally happen when databases and
                projects are in geographically distant regions. This can cause
                API and subsequently application performance issues. We want
                your GraphQL APIs to be <b>lightning fast</b>, therefore we
                recommend that you either deploy your Hasura project in the same
                region as your database or select a database instance
                that&apos;s closer to where you&apos;ve deployed Hasura.
                <LearnMoreLink href="https://hasura.io/docs/latest/projects/regions/#changing-region-of-an-existing-project" />
              </span>
              <div className="flex items-center flex-row ml-xs">
                <Button
                  className="mr-xs"
                  onClick={() => {
                    refetch();
                  }}
                  isLoading={databaseCheckLoading}
                  loadingText="Measuring Latencies..."
                  icon={<FaRedoAlt />}
                >
                  Re-check Database Latency
                </Button>
                <Button
                  className="mr-xs"
                  onClick={() => openUpdateProjectRegionPage(rowId)}
                  icon={<FaExternalLinkAlt />}
                >
                  Update Project Region
                </Button>
              </div>
            </div>
          </IndicatorCard>
        </div>
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
