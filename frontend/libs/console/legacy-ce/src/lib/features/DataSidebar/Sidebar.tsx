import clsx from 'clsx';
import { useCallback, useMemo, useRef, useState } from 'react';
import globals from '../../Globals';

import { Badge } from '../../new-components/Badge';
import { Button } from '../../new-components/Button';
import { usePushRoute } from '../ConnectDBRedesign/hooks';
import { hasNameAndSchema } from '../Data/ManageTable/utils';
import { useMetadata } from '../hasura-metadata-api';
import { NavTree } from './NavTree/NavTree';

import { useDispatch } from 'react-redux';
import { UPDATE_CURRENT_DATA_SOURCE } from '../../components/Services/Data/DataActions';
import { LeafType } from './NavTree/types';
import { SidebarContext } from './SidebarContext';
import { SidebarLink } from './SidebarLink';
import { DATABASE_HEADER_ID, Links, SIDEBAR_LINKS_ID } from './constants';
import { styles } from './styles';
//import { SidebarLinkType } from './types';
import { UseQueryResult } from 'react-query';
import { APIError } from '../../hooks/error';
import { ReactQueryStatusUI } from '../Data/components';
import { Source } from '../hasura-metadata-types';
import {
  manageDatabaseUrl,
  manageFunctionUrl,
  manageTableUrl,
} from './navigation-utils';

const DataProvider = () => {
  const {
    status,
    error,
    refetch,
    isRefetching,
    data: sources = [],
  } = useMetadata(m => m.metadata.sources);

  return (
    <SidebarUI
      metadataQueryStatus={status}
      metadataError={error}
      isRefetching={isRefetching}
      onRetryMetadata={() => refetch()}
      sources={sources}
    />
  );
};
// data bound component:
export const Sidebar = DataProvider;

// isolated UI component:
export const SidebarUI = ({
  metadataQueryStatus: status,
  metadataError,
  sources,
  isRefetching,
  onRetryMetadata,
}: {
  metadataQueryStatus: UseQueryResult['status'];
  metadataError: APIError | null;
  isRefetching: boolean;
  onRetryMetadata: () => void;
  sources: Source[];
}) => {
  const isLoading =
    status === 'loading' || (status === 'error' && isRefetching);

  // these may not have a use case anymore since we aren't calling legacy loading
  // but these can be used to flip a loading state on any tree item
  const [loadingSources] = useState<string[]>([]);
  const [loadingItems] = useState<string[]>([]);
  const dispatch = useDispatch();

  const push = usePushRoute();

  // const dispatch = useDispatch();

  const links = useMemo(
    () =>
      Links.filter(link => {
        if (link?.cliOnly && globals.consoleMode !== 'cli') {
          return false;
        }

        if (link?.hideIfNoSources && sources.length === 0) {
          return false;
        }

        return true;
      }),
    [sources]
  );

  const lastSource = useRef<string | undefined>(undefined);
  const lastSchema = useRef<string | undefined>(undefined);

  const handleDatabaseClick = useCallback(
    (clickedSource: string) => {
      // this makes sure the redux state knows we switched sources. this matters for the run sql page
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: clickedSource,
      });

      push(manageDatabaseUrl(clickedSource));
    },
    [dispatch, push]
  );

  const handleDatabaseObjectClick = useCallback(
    async ({ dataSourceName, ...details }: LeafType) => {
      const metadataSource = sources.find(s => s.name === dataSourceName);

      if (!metadataSource) {
        throw new Error(`Source ${dataSourceName} not found in metadata`);
      }

      const entity = 'table' in details ? details.table : details.function;

      const entityKind =
        'table' in details ? ('table' as const) : ('function' as const);

      if (hasNameAndSchema(entity)) lastSchema.current = entity.schema;

      if (entityKind === 'table') {
        push(manageTableUrl({ dataSourceName, table: entity }));
      } else {
        push(manageFunctionUrl({ dataSourceName, fn: entity }));
      }

      lastSource.current = dataSourceName;
    },
    [push, sources]
  );

  return (
    <SidebarContext.Provider value={{ loadingSources, loadingItems }}>
      <div className={styles.outerContainer}>
        <div id={SIDEBAR_LINKS_ID} className={clsx(styles.linksContainer)}>
          {status === 'error' && (
            <div className="p-4 flex flex-col gap-2">
              There was an error fetching metadata:
              <ReactQueryStatusUI error={metadataError} status={status} />
              <Button
                className="self-center"
                onClick={() => onRetryMetadata()}
                isLoading={isLoading}
                loadingText={'Refetching Metadata'}
              >
                Retry Loading Metadata
              </Button>
            </div>
          )}
          {status !== 'error' &&
            links.map(link => (
              <SidebarLink
                {...link}
                isLoading={isLoading}
                key={link.to}
                active={link.isLinkActive()}
                dataTest={link.name.replace(' ', '-')}
              />
            ))}
        </div>
        <div
          id={DATABASE_HEADER_ID}
          className="w-full flex text-lg items-center px-4 py-3 bg-slate-600 text-white h-[45px]"
        >
          <div className="flex items-center justify-between w-full">
            <span>
              Databases
              <Badge
                className={clsx(
                  'transition-all opacity-0 ml-2',
                  status === 'success' && 'opacity-100'
                )}
                color="purple"
              >
                {sources.length}
              </Badge>
            </span>
            <div>
              <Button
                isLoading={isLoading}
                loadingText={'Loading Sources'}
                onClick={() => push('/data/manage')}
              >
                Manage
              </Button>
            </div>
          </div>
        </div>
        <div>
          {status === 'success' && (
            <NavTree
              handleDatabaseClick={handleDatabaseClick}
              handleDatabaseObjectClick={handleDatabaseObjectClick}
            />
          )}
        </div>
      </div>
    </SidebarContext.Provider>
  );
};
