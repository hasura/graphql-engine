import React from 'react';
import { Link } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';
import { RightContainer } from '../../Common/Layout/RightContainer';
import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import { ReduxState } from '../../../types';

import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import styles from '../../Common/TableCommon/Table.scss';

const helmetTitle = 'Support Forums | Hasura';

const SupportContainer: React.FC<InjectedProps> = props => {
  const { children } = props;

  const leftContainer = (
    <LeftContainer>
      <ul>
        <li role="presentation" className={styles.active}>
          <Link className={styles.linkBorder} to="/support/forum/">
            Support Forums
          </Link>
        </li>
      </ul>
    </LeftContainer>
  );

  return (
    <PageContainer helmet={helmetTitle} leftContainer={leftContainer}>
      <RightContainer>{children}</RightContainer>
    </PageContainer>
  );
};

const connector = connect(
  (state: ReduxState) => ({
    serverVersion: state.main.serverVersion,
  }),
  mapDispatchToPropsEmpty
);

type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedSupportContainer = connector(SupportContainer);

export default ConnectedSupportContainer;
