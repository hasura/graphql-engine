import * as React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { isCloudConsole, hasLuxFeatureAccess } from '@/utils/cloudConsole';
import Globals from '../../../../../Globals';
import Heroku from './Heroku';
import { HerokuSession } from './Heroku/types';
import { ReduxState } from '../../../../../types';
import styles from './styles.module.scss';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import Tabbed from '../TabbedDataSourceConnection';
import { NotFoundError } from '../../../../Error/PageNotFound';
import { getDataSources } from '../../../../../metadata/selector';
import { HerokuBanner } from './Neon/components/HerokuBanner/Banner';
import { Neon } from './Neon';
import _push from '../../push';

interface Props extends InjectedProps {}

const CreateDataSource: React.FC<Props> = ({
  herokuSession,
  dispatch,
  allDataSources,
}) => {
  // this condition fails for everything other than a Hasura Cloud project
  if (!isCloudConsole(Globals)) {
    throw new NotFoundError();
  }

  const showNeonIntegration =
    hasLuxFeatureAccess(Globals, 'NeonDatabaseIntegration') &&
    Globals.neonOAuthClientId &&
    Globals.neonRootDomain;

  return (
    <Tabbed tabName="create">
      <div className={styles.connect_db_content}>
        {showNeonIntegration ? (
          <div className={`${styles.container} mb-md`}>
            <div className="w-full mb-md">
              <Neon
                dbCreationCallback={dataSourceName => {
                  dispatch(_push(`/data/${dataSourceName}`));
                }}
                errorCallback={() => {
                  dispatch(_push('/data/manage/connect'));
                }}
                allDatabases={allDataSources.map(d => d.name)}
              />
            </div>
            <HerokuBanner />
          </div>
        ) : (
          <div className={`${styles.container} mb-md`}>
            <Heroku
              session={herokuSession}
              dispatch={dispatch}
              allDataSources={allDataSources}
            />
          </div>
        )}
      </div>
    </Tabbed>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    herokuSession: state.main.heroku.session as HerokuSession | undefined,
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    allDataSources: getDataSources(state),
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedCreateDataSourcePage = connector(CreateDataSource);
export default ConnectedCreateDataSourcePage;
