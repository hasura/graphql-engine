/* eslint-disable no-underscore-dangle */
import React, { useEffect } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import { ManageAgents } from '../../../../features/ManageAgents';
import { Button } from '../../../../new-components/Button';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';

import styles from './styles.module.scss';
import { ReduxState } from '../../../../types';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { getDataSources } from '../../../../metadata/selector';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import _push from '../push';
import VPCBanner from '../../../Common/VPCBanner/VPCBanner';
import { useVPCBannerVisibility } from './utils';
import { getRoute } from '../../../../utils/getDataRoute';
import { Collapsible } from '../../../../new-components/Collapsible';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { ListConnectedDatabases } from '../../../../features/ConnectDBRedesign';
import { NeonDashboardLink } from '../DataSources/CreateDataSource/Neon/components/NeonDashboardLink';

type ManageDatabaseProps = InjectedProps;

let autoRedirectedToConnectPage = false;

const ManageDatabase: React.FC<ManageDatabaseProps> = ({
  dataSources,
  dispatch,
  inconsistentObjects,
  location,
  dataHeaders,
  sourcesFromMetadata,
}) => {
  useEffect(() => {
    if (sourcesFromMetadata.length === 0 && !autoRedirectedToConnectPage) {
      /**
       * Because the getDataSources() doesn't list the GDC sources, the Data tab will redirect to the /connect page
       * thinking that are no sources available in Hasura, even if there are GDC sources connected to it. Modifying getDataSources()
       * to list gdc sources is a huge task that involves modifying redux state variables.
       * So a quick workaround is to check from the actual metadata if any sources are present -
       * Combined with checks between getDataSources() and metadata -> we know the remaining sources are GDC sources. In such a case redirect to the manage db route
       */
      dispatch(_push('/data/v2/manage/connect'));
      autoRedirectedToConnectPage = true;
    }
  }, [location, dataSources, dispatch, sourcesFromMetadata.length]);

  const { show: shouldShowVPCBanner, dismiss: dismissVPCBanner } =
    useVPCBannerVisibility();

  const crumbs = [
    {
      title: 'Data',
      url: `/data/`,
    },
    {
      title: 'Manage',
      url: '#',
    },
  ];

  const onClickConnectDB = () => {
    dispatch(_push(getRoute().connectDatabase()));
  };

  return (
    <RightContainer>
      <Helmet title="Manage - Data | Hasura" />
      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <div
          className={`container-fluid ${styles.manage_dbs_page} bootstrap-jail`}
          data-test="manage-database-section"
        >
          <BreadCrumb breadCrumbs={crumbs} />

          <div className={styles.padd_top}>
            <div className={`${styles.display_flex} manage-db-header`}>
              <h2
                className={`${styles.headerText} ${styles.display_inline} ${styles.add_mar_right}`}
              >
                Data Manager
              </h2>
              <Button
                mode="primary"
                size="md"
                className={styles.add_mar_right}
                onClick={onClickConnectDB}
              >
                Connect Database
              </Button>
            </div>
            {shouldShowVPCBanner && (
              <VPCBanner className="mt-md" onClose={dismissVPCBanner} />
            )}
          </div>

          <hr className="mt-sm" />

          <div className="mt-sm">
            <ListConnectedDatabases />
          </div>

          <NeonDashboardLink className="mt-lg" />

          <hr className="my-md" />
          <div className="mt-4">
            <Collapsible
              triggerChildren={
                <div className="flex font-bold items-center text-gray-600 text-lg">
                  Data Connector Agents
                  <IconTooltip
                    message={
                      'Data Connector Agents act as an intermediary abstraction between a data source and the Hasura GraphQL Engine.'
                    }
                  />
                </div>
              }
            >
              <ManageAgents />
            </Collapsible>
          </div>
        </div>
      </Analytics>
    </RightContainer>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    dataHeaders: state.tables.dataHeaders,
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    inconsistentObjects: state.metadata.inconsistentObjects,
    location: state?.routing?.locationBeforeTransitions,
    sourcesFromMetadata: state?.metadata?.metadataObject?.sources ?? [],
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseManagePage = connector(ManageDatabase);
export default ConnectedDatabaseManagePage;
