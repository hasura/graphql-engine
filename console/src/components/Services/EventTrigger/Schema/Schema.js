/* eslint-disable space-infix-ops */
/* eslint-disable no-loop-func  */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { loadTriggers } from '../EventActions';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';

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
