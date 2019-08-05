/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { loadTriggers } from '../EventActions';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import TryItOut from '../../Common/Landing/TryItOut';

const appPrefix = globals.urlPrefix + '/events';

class EventTrigger extends Component {
  constructor(props) {
    super(props);
    // Initialize this table
    const dispatch = this.props.dispatch;
    dispatch(loadTriggers([]));
  }

  render() {
    const { dispatch, listingTrigger } = this.props;

    const styles = require('../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');

    const queryDefinition = `mutation {
  insert_user(objects: [{name: "testuser"}] ){
    affected_rows
  }
}`;
    const showIntroSection = !listingTrigger.length;
    const getIntroSection = () => {
      if (!showIntroSection) {
        return null;
      }

      return (
        <div>
          <TopicDescription
            title="What are Event Triggers?"
            imgUrl={`${globals.assetsPath}/common/img/event-trigger.png`}
            imgAlt="Event Triggers"
            description="Hasura can be used to create event triggers on tables. An Event Trigger atomically captures events (insert, update, delete) on a specified table and then reliably calls a webhook that can carry out any custom logic."
          />
          <hr className={styles.clear_fix} />
        </div>
      );
    };

    const getAddBtn = () => {
      const handleClick = e => {
        e.preventDefault();

        dispatch(push(`${appPrefix}/manage/triggers/add`));
      };

      return (
        <Button
          data-test="data-create-trigger"
          color="yellow"
          size="sm"
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
        className={`${styles.padd_left_remove} container-fluid ${
          styles.padd_top
        }`}
      >
        <div className={styles.padd_left}>
          <Helmet title="Event Triggers | Hasura" />
          <div className={styles.display_flex}>
            <h2 className={`${styles.headerText} ${styles.inline_block}`}>
              Event Triggers
            </h2>
            {getAddBtn()}
          </div>
          <hr />

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
            isAvailable={showIntroSection}
          />
        </div>
      </div>
    );
  }
}

EventTrigger.propTypes = {
  schema: PropTypes.array.isRequired,
  untrackedRelations: PropTypes.array.isRequired,
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
  listingTrigger: state.triggers.listingTrigger,
});

const eventTriggerConnector = connect => connect(mapStateToProps)(EventTrigger);

export default eventTriggerConnector;
