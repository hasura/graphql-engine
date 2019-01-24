import React from 'react';
import PropTypes from 'prop-types';

import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import CommonTabLayout from '../../../Layout/CommonTabLayout/CommonTabLayout';

import _push from '../../push';
import { pageTitle, appPrefix } from './constants';

import tabInfo from './tabInfo';
import globals from '../../../../../Globals';

const prefixUrl = globals.urlPrefix + appPrefix;

import ReusableTextAreaWithCopy from '../../../Layout/ReusableTextAreaWithCopy/ReusableTextAreaWithCopy';

import {
  fetchCustomFunction,
  deleteFunctionSql,
  unTrackCustomFunction,
} from '../customFunctionReducer';

import { SET_SQL } from '../../RawSQL/Actions';

class ModifyCustomFunction extends React.Component {
  constructor() {
    super();
    this.state = {};
    this.state.deleteConfirmationError = null;
  }
  componentDidMount() {
    const { functionName, schema } = this.props.params;
    if (!functionName) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([
      this.props.dispatch(fetchCustomFunction(functionName, schema)),
    ]);
  }
  loadRunSQLAndLoadPage() {
    const { functionDefinition } = this.props.functions;
    Promise.all([
      this.props.dispatch({ type: SET_SQL, data: functionDefinition }),
      this.props.dispatch(_push('/sql')),
    ]);
  }
  updateDeleteConfirmationError(data) {
    this.setState({ ...this.state, deleteConfirmationError: data });
  }
  handleUntrackCustomFunction(e) {
    e.preventDefault();
    this.props.dispatch(unTrackCustomFunction());
  }
  handleDeleteCustomFunction(e) {
    e.preventDefault();
    const a = prompt(
      'Are you absolutely sure?\nThis action cannot be undone. This will permanently delete function. Please type "DELETE" (in caps, without quotes) to confirm.\n'
    );
    try {
      if (a && typeof a === 'string' && a.trim() === 'DELETE') {
        this.updateDeleteConfirmationError(null);
        this.props.dispatch(deleteFunctionSql());
      } else {
        // Input didn't match
        // Show an error message right next to the button
        this.updateDeleteConfirmationError('user confirmation error!');
      }
    } catch (err) {
      console.error(err);
    }
  }
  render() {
    const styles = require('./ModifyCustomFunction.scss');
    const {
      functionSchema: schema,
      functionName,
      functionDefinition,
      isRequesting,
      isDeleting,
      isUntracking,
      isFetching,
    } = this.props.functions;

    const { migrationMode } = this.props;

    const baseUrl = `${appPrefix}/schema/${schema}/functions/${functionName}`;

    const generateMigrateBtns = () => {
      return (
        <div className={styles.commonBtn}>
          <button
            className={styles.yellow_button}
            data-test={'custom-function-edit-modify-btn'}
            onClick={this.loadRunSQLAndLoadPage.bind(this)}
          >
            Modify
          </button>
          <button
            className={
              styles.danger_button +
              ' ' +
              styles.white_button +
              ' ' +
              'btn-default'
            }
            onClick={e => {
              e.preventDefault();
              this.handleUntrackCustomFunction(e);
            }}
            disabled={isRequesting || isDeleting || isUntracking}
            data-test={'custom-function-edit-untrack-btn'}
          >
            {isUntracking ? 'Untracking Function...' : 'Untrack Function'}
          </button>
          <button
            className={
              styles.danger_button +
              ' ' +
              styles.red_button +
              ' ' +
              styles.no_mr_right +
              ' ' +
              'btn-danger'
            }
            onClick={e => {
              e.preventDefault();
              this.handleDeleteCustomFunction(e);
            }}
            data-test={'custom-function-edit-delete-btn'}
            disabled={isRequesting || isDeleting || isUntracking}
          >
            {isDeleting ? 'Deleting Function...' : 'Delete Function'}
          </button>
          {this.state.deleteConfirmationError ? (
            <span
              className={styles.delete_confirmation_error}
              data-test="delete-confirmation-error"
            >
              * {this.state.deleteConfirmationError}
            </span>
          ) : null}
        </div>
      );
    };
    const breadCrumbs = [
      {
        title: 'Data',
        url: appPrefix,
      },
      {
        title: 'Schema',
        url: appPrefix + '/schema',
      },
      {
        title: schema,
        url: appPrefix + '/schema/' + schema,
      },
    ];

    if (functionName) {
      breadCrumbs.push({
        title: functionName,
        url: appPrefix + '/schema/' + schema + '/functions/' + functionName,
      });
      breadCrumbs.push({
        title: 'Modify',
        url: '',
      });
    }
    return (
      <div className={'col-xs-8' + ' ' + styles.modifyWrapper}>
        <Helmet
          title={`Edit ${pageTitle} - ${functionName} - ${pageTitle}s | Hasura`}
        />
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="modify"
          heading={functionName}
          tabsInfo={tabInfo}
          breadCrumbs={breadCrumbs}
          baseUrl={baseUrl}
          showLoader={isFetching}
          testPrefix={'functions'}
        />
        <br />
        {/*
        <h4>Function Definition:</h4>
        */}
        <div className={styles.sqlBlock}>
          <ReusableTextAreaWithCopy
            copyText={functionDefinition}
            textLanguage={'sql'}
          />
        </div>
        {migrationMode
          ? [<hr key="modify-custom-function-divider" />, generateMigrateBtns()]
          : null}
      </div>
    );
  }
}

ModifyCustomFunction.propTypes = {
  functions: PropTypes.array.isRequired,
};

export default ModifyCustomFunction;
