import clsx from 'clsx';
import { useCallback, useMemo, useRef, useState } from 'react';
import { CgSpinner } from 'react-icons/cg';
import Skeleton from 'react-loading-skeleton';
import globals from '../../Globals';

import { Badge } from '../../new-components/Badge';
import { Button } from '../../new-components/Button';
import { IndicatorCard } from '../../new-components/IndicatorCard';
import { usePushRoute } from '../ConnectDBRedesign/hooks';
import { hasNameAndSchema } from '../Data/ManageTable/utils';
import { useMetadata } from '../hasura-metadata-api';
import { Source } from '../hasura-metadata-types';
import { NavTree } from './NavTree/NavTree';
import { LeafType } from './NavTree/components/TreeComponent';
import { SidebarContext } from './SidebarContext';
import { SidebarLink } from './SidebarLink';
import { DATABASE_HEADER_ID, Links, SIDEBAR_LINKS_ID } from './constants';
import { styles } from './styles';
import { SidebarLinkType } from './types';

export const Sidebar = ({
  isLinkActive,
  onLinkClick,
}: {
  isLinkActive: (link: SidebarLinkType) => boolean;
  onLinkClick?: (link: SidebarLinkType) => void;
}) => {
  // these may not have a use case anymore since we aren't calling legacy loading
  // but these can be used to flip a loading state on any tree item
  const [loadingSources] = useState<string[]>([]);
  const [loadingItems] = useState<string[]>([]);

  const {
    status,
    error: metadataError,
    refetch,
    data,
  } = useMetadata(m => ({
    sources: m.metadata.sources,
  }));
  const push = usePushRoute();

  // const dispatch = useDispatch();

  const links = useMemo(
    () =>
      Links.filter(link => {
        if (link?.cliOnly && globals.consoleMode !== 'cli') {
          return false;
        }

        if (link?.hideIfNoSources && (data?.sources ?? []).length === 0) {
          return false;
        }

        return true;
      }),
    [data?.sources]
  );

  const handleDatabaseClick = useCallback(
    (clickedSource: string, allSources: Source[]) => {
      push(
        `/data/v2/manage/database?database=${encodeURIComponent(clickedSource)}`
      );
    },
    [push]
  );

  const lastSource = useRef<string | undefined>(undefined);
  const lastSchema = useRef<string | undefined>(undefined);

  const handleTableClick = useCallback(
    async ({ dataSourceName, ...details }: LeafType, allSources: Source[]) => {
      const metadataSource = allSources.find(s => s.name === dataSourceName);

      if (!metadataSource) {
        throw new Error(`Source ${dataSourceName} not found in metadata`);
      }

      const entity = 'table' in details ? details.table : details.function;

      console.log(entity);

      if (hasNameAndSchema(entity)) lastSchema.current = entity.schema;

      push(
        `data/v2/manage/table/browse?database=${dataSourceName}&table=${encodeURIComponent(
          JSON.stringify(entity)
        )}`
      );

      lastSource.current = dataSourceName;
    },
    [push]
  );

  return (
    <SidebarContext.Provider value={{ loadingSources, loadingItems }}>
      <div className={styles.outerContainer}>
        <div id={SIDEBAR_LINKS_ID} className={clsx(styles.linksContainer)}>
          {status === 'loading' && (
            <Skeleton count={3} className={styles.link.default} />
          )}
          {status === 'error' && (
            <IndicatorCard status="negative">
              There was an error fetching metadata:
              <div className="whitespace-pre-wrap text-muted m-2">
                {metadataError?.message ??
                  JSON.stringify(metadataError, null, 2)}
              </div>
              <Button onClick={() => refetch()}>Retry Loading Metadata</Button>
            </IndicatorCard>
          )}
          {status === 'success' &&
            links.map(link => (
              <SidebarLink
                {...link}
                key={link.to}
                active={isLinkActive(link)}
                dataTest={link.name.replace(' ', '-')}
                onClick={() => onLinkClick?.(link)}
              />
            ))}
        </div>
        <div>
          <div
            id={DATABASE_HEADER_ID}
            className="w-full flex text-lg items-center px-4 py-3 bg-slate-600 text-white h-[45px]"
          >
            <span className="mr-2">Databases</span>
            {status === 'success' && (
              <Badge className="text-lg" color="purple">
                {data?.sources.length}
              </Badge>
            )}
            {status === 'loading' && <CgSpinner className="animate-spin" />}
          </div>
          <div>
            {status === 'success' && (
              <NavTree
                handleDatabaseClick={source =>
                  handleDatabaseClick(source, data.sources)
                }
                handleTableClick={table =>
                  handleTableClick(table, data.sources)
                }
              />
            )}
          </div>
        </div>
      </div>
    </SidebarContext.Provider>
  );
};
