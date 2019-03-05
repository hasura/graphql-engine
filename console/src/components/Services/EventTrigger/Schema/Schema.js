/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { loadTriggers } from '../EventActions';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';
import ReusableTextAreaWithCopy from '../../Layout/ReusableTextAreaWithCopy/ReusableTextAreaWithCopy';
import TopicDescription from '../../CommonLanding/TopicDescription';
import TryItOut from '../../CommonLanding/TryItOut';
const appPrefix = globals.urlPrefix + '/events';

class Schema extends Component {
  constructor(props) {
    super(props);
    // Initialize this table
    const dispatch = this.props.dispatch;
    dispatch(loadTriggers());
  }

  render() {
    const { migrationMode, dispatch } = this.props;

    const styles = require('../PageContainer/PageContainer.scss');
    const queryDefinition = 'mutation{ insert_user()}';
    const footerEvent = (<span>Head to the Events tab and see an event invoked under <span className={styles.fontWeightBold}> test-trigger</span>.</span>);
    return (
      <div
        className={`${styles.padd_left_remove} container-fluid ${
          styles.padd_top
        }`}
      >
        <div className={styles.padd_left}>
          <Helmet title="Event Triggers | Hasura" />
          <div className={styles.display_flex}>
            <h2 className={`${styles.headerText} ${styles.addPaddRight} ${
              styles.inline_block
              }`}>
              Event Triggers{' '}
            </h2>
            {migrationMode ? (
              <Button
                data-test="data-create-trigger"
                color="yellow"
                size="sm"
                onClick={e => {
                  e.preventDefault();
                  dispatch(push(`${appPrefix}/manage/triggers/add`));
                }}
              >
                Add
              </Button>
            ) : null}
          </div>
          <hr />
          <TopicDescription
            title="What are Event Triggers?"
            imgUrl="https://storage.googleapis.com/hasura-graphql-engine/console/assets/event-trigger.png"
            imgAlt="Event Triggers"
            description="Hasura can be used to create event triggers on tables. An Event Trigger atomically captures events (insert, update, delete) on a specified table and then reliably calls a webhook that can carry out any custom logic."
          />
          <hr className={styles.clear_fix} />
          <TryItOut
            service="eventTrigger"
            title= "Click to deploy an example Event Trigger to Glitch"
            queryDefinition="mutation{ insert_user()}"
            footerDescription={footerEvent}
            glitchLink="https://github.com/hasura/graphql-engine/tree/master/community"
            googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community"
            MicrosoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community"
            awsLink="https://github.com/hasura/graphql-engine/tree/master/community"
            adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community"
          />
        </div>
      </div>
    );
  }
}

Schema.propTypes = {
  schema: PropTypes.array.isRequired,
  untracked: PropTypes.array.isRequired,
  untrackedRelations: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  schema: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  untracked: state.tables.untrackedSchemas,
  migrationMode: state.main.migrationMode,
  untrackedRelations: state.tables.untrackedRelations,
  currentSchema: state.tables.currentSchema,
});

const schemaConnector = connect => connect(mapStateToProps)(Schema);

export default schemaConnector;
