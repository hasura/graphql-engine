import React from 'react';
import { Link } from 'react-router';

import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import EventSubSidebar from './EventSubSidebar';
import { connect } from 'react-redux';

const appPrefix = '/events';

const EventPageContainer = ({
  schema,
  currentSchema,
  children,
  location,
  dispatch,
}) => {
  const styles = require('../../Common/TableCommon/Table.scss');

  const currentLocation = location.pathname;

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.includes('events/manage') ? styles.active : ''
        }
      >
        <Link className={styles.linkBorder} to={appPrefix + '/manage'}>
          Manage
        </Link>
        <EventSubSidebar
          location={location}
          schema={schema}
          currentSchema={currentSchema}
          dispatch={dispatch}
        />
      </li>
    </ul>
  );

  const helmet = 'Events | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps = state => {
  return {
    schema: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
  };
};

const ConnectedEventPageContainer = connect(mapStateToProps)(
  EventPageContainer
);
export default ConnectedEventPageContainer;
