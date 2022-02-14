import * as React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import Globals from '../../../../../Globals';
import Heroku from './Heroku';
import { HerokuSession } from './Heroku/types';
import { ReduxState } from '../../../../../types';
import styles from './styles.scss';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import Tabbed from '../TabbedDataSourceConnection';
import { NotFoundError } from '../../../../Error/PageNotFound';
import { getDataSources } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const CreateDataSource: React.FC<Props> = ({
  herokuSession,
  dispatch,
  allDataSources,
}) => {
  if (!Globals.herokuOAuthClientId || !Globals.hasuraCloudTenantId) {
    throw new NotFoundError();
  }

  return (
    <Tabbed tabName="create">
      <div className={styles.connect_db_content}>
        <div className={`${styles.container}`}>
          <Heroku
            session={herokuSession}
            dispatch={dispatch}
            allDataSources={allDataSources}
          />
        </div>
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
