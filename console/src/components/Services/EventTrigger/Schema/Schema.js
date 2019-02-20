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
    return (
      <div
        className={`${styles.padd_left_remove} container-fluid ${
          styles.padd_top
        }`}
      >
        <div className={styles.padd_left}>
          <Helmet title="Event Triggers | Hasura" />
          <div>
            <h2 className={`${styles.heading_text} ${styles.inline_block}`}>
              {' '}
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
                Create Trigger
              </Button>
            ) : null}
          </div>
          <hr />
          <p>
            1. Head to the Data tab and create a table, say `user`, with columns
            `id` and `name`
          </p>
          <p>
            2.
            <a
              className={styles.add_mar_left_small}
              href="https://glitch.com/edit/#!/hasura-sample-event-trigger"
              target="_blank"
            >
              <button className={'btn btn-sm ' + styles.yellow_button}>
                Deploy with Glitch
              </button>
            </a>
            <span className={styles.add_pad_left}>
              Click to deploy an example Event Trigger to Glitch
            </span>
          </p>
          <p>3. Add the Event Trigger</p>
          <p className={styles.add_pad_left}>
            - Click on the SHOW button in the Glitch console and copy the URL
          </p>
          <p className={styles.add_pad_left}>
            - Create an event trigger by clicking on the <b>Create Trigger</b>{' '}
            button at the top of this page.
          </p>
          <p className={styles.add_pad_left}>
            - Set the name as "sample-trigger". Choose `user` table and select
            `INSERT`, `UPDATE` and `DELETE` operations. <br />
            Enter the above URL as <b>WEBHOOK URL</b>
          </p>
          <p className={styles.add_pad_left}>
            - Click on the <b>Create</b> button - That's it!
          </p>
          <p>4. Head to the GraphiQL tab and try out the following query:</p>
          <ReusableTextAreaWithCopy
            copyText={queryDefinition}
            textLanguage={'graphql'}
          />
          <p className={styles.add_pad_top}>
            Head to the Events tab and see an event invoked under
            `sample-trigger`.
          </p>
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
