import React from 'react';
import { Link } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
// import LeftSidebar from './Sidebar';

import { MapStateToProps } from '../../../types';

import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import styles from '../../Common/TableCommon/Table.scss';

interface Props extends InjectedProps {}

const Container: React.FC<Props> = props => {
  const { children } = props;

  const sidebarContent = (
    <ul>
      <li role="presentation" className={styles.active}>
        <Link className={styles.linkBorder} to="/support/forum">
          Support Forums
        </Link>
      </li>
    </ul>
  );

  const helmetTitle = 'Support Forums | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmetTitle} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps: MapStateToProps<PropsFromState> = state => {
  return {
    serverVersion: state.main.serverVersion,
  };
};

type PropsFromState = {
  serverVersion: string;
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ContainerConnector = connector(Container);

export default ContainerConnector;
