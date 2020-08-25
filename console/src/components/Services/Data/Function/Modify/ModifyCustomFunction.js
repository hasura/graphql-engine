import React from 'react';
import PropTypes from 'prop-types';

import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import CommonTabLayout from '../../../../Common/Layout/CommonTabLayout/CommonTabLayout';

import { pageTitle, appPrefix } from './constants';

import tabInfo from './tabInfo';
import globals from '../../../../../Globals';
import Button from '../../../../Common/Button/Button';
import styles from './ModifyCustomFunction.scss';

const prefixUrl = globals.urlPrefix + appPrefix;

import TextAreaWithCopy from '../../../../Common/TextAreaWithCopy/TextAreaWithCopy';

import {
  fetchCustomFunction,
  deleteFunctionSql,
  unTrackCustomFunction,
  updateSessVar,
} from '../customFunctionReducer';

import { NotFoundError } from '../../../../Error/PageNotFound';
import { getConfirmation } from '../../../../Common/utils/jsUtils';
import {
  getFunctionBaseRoute,
  getSchemaBaseRoute,
} from '../../../../Common/utils/routesUtils';
import SessionVarSection from './SessionVarSection';
import RawSqlButton from '../../Common/Components/RawSqlButton';

class ModifyCustomFunction extends React.Component {
  constructor() {
    super();

    this.state = {
      deleteConfirmationError: null,
      funcFetchCompleted: false,
    };

    this.handleUntrackCustomFunction = this.handleUntrackCustomFunction.bind(
      this
    );
    this.handleDeleteCustomFunction = this.handleDeleteCustomFunction.bind(
      this
    );
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

  UNSAFE_componentWillReceiveProps(nextProps) {
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

  onSessVarUpdate = sessVar => this.props.dispatch(updateSessVar(sessVar));

  updateDeleteConfirmationError(data) {
    this.setState({ deleteConfirmationError: data });
  }

  handleUntrackCustomFunction(e) {
    e.preventDefault();

    const functionName = this.props.functions.functionName;

    const confirmMessage = `This will remove the function "${functionName}" from the GraphQL schema`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      this.props.dispatch(unTrackCustomFunction());
    }
  }

  handleDeleteCustomFunction(e) {
    e.preventDefault();

    const functionName = this.props.functions.functionName;

    const confirmMessage = `This will permanently delete the function "${functionName}" from the database`;
    const isOk = getConfirmation(confirmMessage, true, functionName);

    if (isOk) {
      try {
        this.updateDeleteConfirmationError(null);
        this.props.dispatch(deleteFunctionSql());
      } catch (err) {
        console.error('Delete custom function error: ', err);
      }
    }
  }

  render() {
    const {
      functionSchema: schema,
      functionName,
      functionDefinition,
      isRequesting,
      isDeleting,
      isUntracking,
      isFetching,
      configuration,
    } = this.props.functions;

    if (this.state.funcFetchCompleted && !functionName) {
      // throw a 404 exception
      throw new NotFoundError();
    }
    const loading =
      isRequesting || isDeleting || isUntracking || isFetching
        ? { isRequesting, isDeleting, isUntracking, isFetching }
        : null;

    const { migrationMode, dispatch } = this.props;

    const functionBaseUrl = getFunctionBaseRoute(schema, functionName);

    const generateMigrateBtns = () => {
      return (
        <div className={styles.commonBtn}>
          <Button
            color="white"
            className={styles.add_mar_right}
            onClick={this.handleUntrackCustomFunction}
            disabled={loading}
            data-test={'custom-function-edit-untrack-btn'}
          >
            {loading?.isUntracking
              ? 'Untracking Function...'
              : 'Untrack Function'}
          </Button>
          <Button
            color="red"
            onClick={this.handleDeleteCustomFunction}
            data-test={'custom-function-edit-delete-btn'}
            disabled={loading}
          >
            {loading?.isDeleting ? 'Deleting Function...' : 'Delete Function'}
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
        url: getSchemaBaseRoute(schema),
      },
    ];

    if (functionName) {
      breadCrumbs.push({
        title: functionName,
        url: functionBaseUrl,
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
          baseUrl={functionBaseUrl}
          showLoader={isFetching}
          testPrefix={'functions'}
        />
        <br />
        <div className={`${styles.display_flex}`}>
          <h4 className={styles.subheading_text}>
            Function Definition:
            <span className={styles.add_mar_left}>
              <RawSqlButton
                className={styles.add_mar_right}
                sql={functionDefinition}
                dispatch={dispatch}
                data-test="modify-view"
              >
                Modify
              </RawSqlButton>
            </span>
          </h4>
        </div>

        <div className={styles.sqlBlock}>
          <TextAreaWithCopy
            copyText={functionDefinition}
            textLanguage={'sql'}
            id="copyCustomFunctionSQL"
          />
        </div>
        <SessionVarSection
          key={functionName}
          functionName={functionName}
          configuration={configuration}
          loading={loading}
          onSessVarUpdate={this.onSessVarUpdate}
        />
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
