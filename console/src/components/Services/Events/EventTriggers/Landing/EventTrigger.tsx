/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import React from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import globals from '../../../../../Globals';
import Button from '../../../../Common/Button/Button';
import TopicDescription from '../../../Common/Landing/TopicDescription';
import { getAddETRoute } from '../../../../Common/utils/routesUtils';
import {
  getReactHelmetTitle,
  mapDispatchToPropsEmpty,
} from '../../../../Common/utils/reactUtils';

import TryItOut from '../../../Common/Landing/TryItOut';
import styles from '../../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';
import { EVENTS_SERVICE_HEADING, EVENT_TRIGGER } from '../../constants';
import _push from '../../../Data/push';

interface Props extends InjectedProps {}

const EventTrigger: React.FC<Props> = props => {
  const { dispatch } = props;

  const queryDefinition = `mutation {
insert_user(objects: [{name: "testuser"}] ){
  affected_rows
}
}`;
  const getIntroSection = () => {
    return (
      <div>
        <TopicDescription
          title={`What are ${EVENT_TRIGGER}s?`}
          imgUrl={`${globals.assetsPath}/common/img/event-trigger.png`}
          imgAlt={`${EVENT_TRIGGER}s`}
          description={`An ${EVENT_TRIGGER} atomically captures events (insert, update, delete) on a specified table and then reliably calls a HTTP webhook to run some custom business logic.`}
          knowMoreHref="https://hasura.io/docs/latest/graphql/core/event-triggers/index.html"
        />
        <hr className={`${styles.clear_fix} my-lg`} />
      </div>
    );
  };

  const getAddBtn = () => {
    const handleClick = (e: React.MouseEvent<HTMLButtonElement>) => {
      e.preventDefault();
      dispatch(_push(getAddETRoute()));
    };

    return (
      <Button
        data-test="data-create-trigger"
        color="yellow"
        size="sm"
        type="submit"
        className={styles.add_mar_left}
        onClick={handleClick}
      >
        Create
      </Button>
    );
  };

  const footerEvent = (
    <span>
      Head to the Events tab and see an event invoked under{' '}
      <span className={styles.fontWeightBold}> test-trigger</span>.
    </span>
  );

  return (
    <div
      className={`${styles.padd_left_remove} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <Helmet
          title={getReactHelmetTitle(EVENT_TRIGGER, EVENTS_SERVICE_HEADING)}
        />
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.inline_block}`}>
            {EVENT_TRIGGER}s
          </h2>
          {getAddBtn()}
        </div>
        <hr className="my-md" />

        {getIntroSection()}

        <TryItOut
          service="eventTrigger"
          title="Steps to deploy an example Event Trigger to Glitch"
          queryDefinition={queryDefinition}
          footerDescription={footerEvent}
          glitchLink="https://glitch.com/edit/#!/hasura-sample-event-trigger"
          googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/google-cloud-functions/nodejs8"
          MicrosoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/azure-functions/nodejs"
          awsLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/aws-lambda/nodejs8"
          adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/"
        />
      </div>
    </div>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;

const ETConnector = connector(EventTrigger);
export default ETConnector;
