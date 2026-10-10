import DataSourceItem from './DataSourceItem';
import RemoteSchemaItem from './RemoteSchemaItem';
import ActionsItem from './ActionsItem';
import OverallHealthCard from './OverallHealthCard';
import styles from '../MetricsV1.module.scss';
import AccessDenied from '../../../AccessDenied/AccessDenied';
import clsx from 'clsx';
import { useProjectInfo } from '../../../../hooks/useProjectInfo';
import { useAuthContext } from '../../../../shared/auth/context';
import { useInconsistentMetadata, useMetadata } from '@hasura/metadata/api';
import { hasAdminAccess } from '@hasura/console-legacy-ce';

const SourceHealth = () => {
  const { data: projectInfo } = useProjectInfo();
  const { privileges } = useAuthContext();
  const { data: inconsistentMetadata } = useInconsistentMetadata();
  const { data: metadataData } = useMetadata();

  const isAdmin = hasAdminAccess(projectInfo?.privileges ?? privileges ?? []);

  return (
    <div className={clsx(styles['sourceHealth'], 'bootstrap-jail')}>
      <div
        className={clsx(
          'w-full',
          isAdmin ? 'lg:w-8/12' : 'lg:w-full',
          styles['no_pad'],
        )}
      >
        <div>
          <p className="font-bold">Source Health</p>
          {isAdmin ? (
            <ul
              className={`${styles['tree']} ${styles['ul_pad_remove']} ${styles['horizontal']}`}
            >
              <li>
                <OverallHealthCard />
                <ul className={styles['ul_pad_remove']}>
                  {metadataData?.metadata?.sources
                    ? metadataData.metadata.sources.map((source, ix) => (
                        <DataSourceItem
                          key={`DataSource_${
                            source?.name ||
                            Math.floor(Math.random() * ix * 1000)
                          }`}
                          source={source}
                          inconsistentObjects={
                            inconsistentMetadata?.inconsistent_objects ?? []
                          }
                        />
                      ))
                    : null}
                  {metadataData?.metadata?.remote_schemas
                    ? metadataData.metadata.remote_schemas.map((source, ix) => (
                        <RemoteSchemaItem
                          source={source}
                          inconsistentObjects={
                            inconsistentMetadata?.inconsistent_objects ?? []
                          }
                          key={`RemoteSchema_${
                            source.name || Math.floor(Math.random() * ix * 1000)
                          }`}
                        />
                      ))
                    : null}
                  {Array.isArray(metadataData?.metadata?.actions) &&
                    metadataData.metadata.actions.length > 0 && (
                      <ActionsItem
                        actions={metadataData.metadata.actions.map(
                          (action) => action.name,
                        )}
                      />
                    )}
                </ul>
              </li>
            </ul>
          ) : (
            <AccessDenied alignCenter={false} />
          )}
        </div>
      </div>
    </div>
  );
};

export default SourceHealth;
