import React from 'react';
import PropTypes from 'prop-types';

import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';

import _push from '../../push';
import { pageTitle, appPrefix } from './constants';

import tabInfo from './tabInfo';
import globals from '../../../../../Globals';
import Button from '../../../../Common/Button/Button';

const prefixUrl = globals.urlPrefix + appPrefix;

import TextAreaWithCopy from '../../../../Common/TextAreaWithCopy/TextAreaWithCopy';

import {
  fetchCustomFunction,
  deleteFunctionSql,
  unTrackCustomFunction,
} from '../customFunctionReducer';

import { SET_SQL } from '../../RawSQL/Actions';
import { NotFoundError } from '../../../../Error/PageNotFound';

class ModifyCustomFunction extends React.Component {
  constructor() {
    super();

    this.state = {
      deleteConfirmationError: null,
      funcFetchCompleted: false,
    };
  }

  componentDidMount() {
    const { functionName, schema } = this.props.params;
    if (!functionName || !schema) {
      this.props.dispatch(push(prefixUrl));
    }
    Promise.all([
      this.props
        .dispatch(fetchCustomFunction(functionName, schema))
        .then(() => {
          this.setState({ funcFetchCompleted: true });
        }),
    ]);
  }

  componentWillReceiveProps(nextProps) {
    const { functionName, schema } = this.props.params;
    if (
      functionName !== nextProps.params.functionName ||
      schema !== nextProps.params.schema
    ) {
      Promise.all([
        this.props
          .dispatch(
            fetchCustomFunction(
              nextProps.params.functionName,
              nextProps.params.schema
            )
          )
          .then(() => {
            this.setState({ funcFetchCompleted: true });
          }),
      ]);
    }
  }

  loadRunSQLAndLoadPage() {
    const { functionDefinition } = this.props.functions;
    Promise.all([
      this.props.dispatch({ type: SET_SQL, data: functionDefinition }),
      this.props.dispatch(_push('/sql')),
    ]);
  }

  updateDeleteConfirmationError(data) {
    this.setState({ deleteConfirmationError: data });
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

    if (this.state.funcFetchCompleted && !functionName) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const { migrationMode } = this.props;

    const baseUrl = `${appPrefix}/schema/${schema}/functions/${functionName}`;

    const generateMigrateBtns = () => {
      return (
        <div className={styles.commonBtn}>
          <Button
            color="yellow"
            className={styles.add_mar_right}
            data-test={'custom-function-edit-modify-btn'}
            onClick={this.loadRunSQLAndLoadPage.bind(this)}
          >
            Modify
          </Button>
          <Button
            color="white"
            className={styles.add_mar_right}
            onClick={e => {
              e.preventDefault();
              this.handleUntrackCustomFunction(e);
            }}
            disabled={isRequesting || isDeleting || isUntracking}
            data-test={'custom-function-edit-untrack-btn'}
          >
            {isUntracking ? 'Untracking Function...' : 'Untrack Function'}
          </Button>
          <Button
            color="red"
            onClick={e => {
              e.preventDefault();
              this.handleDeleteCustomFunction(e);
            }}
            data-test={'custom-function-edit-delete-btn'}
            disabled={isRequesting || isDeleting || isUntracking}
          >
            {isDeleting ? 'Deleting Function...' : 'Delete Function'}
          </Button>
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
          <TextAreaWithCopy
            copyText={functionDefinition}
            textLanguage={'sql'}
            id={'copyCustomFunctionSQL'}
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
