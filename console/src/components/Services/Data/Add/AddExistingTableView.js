import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';

import {
  setTableName,
  setDefaults,
  addExistingTableSql,
} from './AddExistingTableViewActions';
import { Heading } from '../../../UIKit/atoms';
import styles from '../../../Common/TableCommon/Table.scss';

class AddExistingTableView extends Component {
  constructor(props) {
    super(props);
    this.props.dispatch(setDefaults());
  }

  render() {
    const { dispatch, ongoingRequest, lastError, lastSuccess } = this.props;

    let alert = null;
    if (ongoingRequest) {
      alert = (
        <div className="col-xs-8 hidden">
          <div className="hidden alert alert-warning" role="alert">
            Creating...
          </div>
        </div>
      );
    } else if (lastError) {
      alert = (
        <div className="col-xs-8 hidden">
          <div className="hidden alert alert-danger" role="alert">
            Error: {JSON.stringify(lastError)}
          </div>
        </div>
      );
    } else if (lastSuccess) {
      alert = (
        <div className="col-xs-8 hidden">
          <div className="hidden alert alert-success" role="alert">
            Created! Redirecting...
          </div>
        </div>
      );
    }

    return (
      <div
        className={
          'container-fluid ' + styles.clear_fix + ' ' + styles.padd_top
        }
      >
        <Helmet title="Add Existing Table/View - Data | Hasura" />
        <div className={styles.subHeader}>
          <Heading as="h2" fontSize="18px" pb="20px">
            Add an existing table or view
          </Heading>
          <div className="clearfix" />
        </div>
        <div className={'container-fluid ' + styles.padd_left_remove}>
          {alert}
          <div
            className={
              styles.addCol +
              ' col-xs-6 ' +
              styles.padd_left_remove +
              ' ' +
              styles.padd_bottom
            }
          >
            <Heading as="h4" fontSize="15px" pb="20px" mt="0px" mb="0px">
              Table/View name:
            </Heading>
            <form
              onSubmit={e => {
                e.preventDefault();
                dispatch(addExistingTableSql());
              }}
            >
              <input
                type="text"
                className={styles.tableNameInput + ' form-control'}
                onChange={e => {
                  dispatch(setTableName(e.target.value));
                }}
                data-test="existing-table"
              />
              <hr />
              <input
                value="Add"
                type="submit"
                className={'btn ' + styles.yellow_button}
                data-test="add-existing-table-button"
              />
            </form>
            <div />
          </div>
        </div>
      </div>
    );
  }
}

AddExistingTableView.propTypes = {
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.addTable.existingTableView,
  };
};

const addExistingTableViewConnector = connect =>
  connect(mapStateToProps)(AddExistingTableView);

export default addExistingTableViewConnector;
