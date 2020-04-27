import React from 'react';
import { Link } from 'react-router';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/Sidebar';
import styles from '../../../Common/TableCommon/Table.scss';
import { Triggers } from '../Types';
import { appPrefix } from '../constants';
import { TriggersState } from '../state';

interface TriggersContainerProps extends React.ComponentProps<'div'> {
  triggers: Triggers;
  location: {
    pathname: string;
  };
}

const Container: React.FC<TriggersContainerProps> = props => {
  const { triggers, children, location } = props;

  const currentLocation = location.pathname;

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.includes('triggers/event') ? styles.active : ''
        }
      >
        <Link className={styles.linkBorder} to={appPrefix + '/events/manage'}>
          Event Triggers
        </Link>
        <LeftSidebar
          appPrefix={appPrefix}
          triggers={triggers.event}
          currentTrigger={{ name: '' }}
          service={'event triggers'}
        />
      </li>
      <li
        role="presentation"
        className={
          currentLocation.includes('triggers/scheduled') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={appPrefix + '/scheduled/manage'}
        >
          Scheduled Triggers
        </Link>
        <LeftSidebar
          appPrefix={appPrefix}
          triggers={triggers.scheduled}
          currentTrigger={{ name: '' }}
          service={'scheduled triggers'}
        />
      </li>
    </ul>
  );

  const helmetTitle = 'Triggers | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmetTitle} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps = (state: { triggers: TriggersState }) => {
  return {
    ...state.triggers,
  };
};

export default (connect: any) => connect(mapStateToProps)(Container);
