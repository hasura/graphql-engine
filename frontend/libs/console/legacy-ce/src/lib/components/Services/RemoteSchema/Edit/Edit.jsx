import React from 'react';
import {
  Analytics,
  REDACT_EVERYTHING,
  getAnalyticsAttributes,
} from '../../../../features/Analytics';
import Common from '../Common/Common';

import {
  fetchRemoteSchema,
  deleteRemoteSchema,
  modifyRemoteSchema,
  RESET,
  TOGGLE_MODIFY,
  getHeaderEvents,
} from '../Add/addRemoteSchemaReducer';
import { VIEW_REMOTE_SCHEMA } from '../Actions';
import { push } from 'react-router-redux';
import Helmet from 'react-helmet';
import { Button } from '../../../../new-components/Button';

import { appPrefix, pageTitle } from '../constants';

import globals from '../../../../Globals';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { getRemoteSchemasSelector } from '../../../../metadata/selector';
import { Tabs } from '../Common/Tabs';
import { InconsistentBadge } from '../Common/GraphQLCustomization/InconsistentBadge';

const prefixUrl = globals.urlPrefix + appPrefix;
class Edit extends React.Component {
  constructor() {
    super();
    this.editClicked = this.editClicked.bind(this);
    this.modifyClick = this.modifyClick.bind(this);
    this.handleDeleteRemoteSchema = this.handleDeleteRemoteSchema.bind(this);
    this.handleCancelModify = this.handleCancelModify.bind(this);

    this.state = {};
    this.state.deleteConfirmationError = null;
  }

  componentDidMount() {
    const { remoteSchemaName } = this.props.params;
    if (!remoteSchemaName) {
      this.props.dispatch(push(prefixUrl));
    }

    Promise.all([
      this.props.dispatch(fetchRemoteSchema(remoteSchemaName)),
      this.props.dispatch({ type: VIEW_REMOTE_SCHEMA, data: remoteSchemaName }),
    ]);
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.params.remoteSchemaName !== this.props.params.remoteSchemaName
    ) {
      Promise.all([
        this.props.dispatch(
          fetchRemoteSchema(this.props.params.remoteSchemaName)
        ),
        this.props.dispatch({
          type: VIEW_REMOTE_SCHEMA,
          data: this.props.params.remoteSchemaName,
        }),
      ]);
    }
  }

  componentWillUnmount() {
    Promise.all([
      this.props.dispatch({ type: RESET }),
      this.props.dispatch({
        type: getHeaderEvents.UPDATE_HEADERS,
        data: [
          {
            name: '',
            type: 'static',
            value: '',
          },
        ],
      }),
      this.props.dispatch({ type: VIEW_REMOTE_SCHEMA, data: '' }),
    ]);
  }

  handleDeleteRemoteSchema(e) {
    e.preventDefault();

    const remoteSchemaName = this.props.params.remoteSchemaName;

    const confirmMessage = `This will remove the remote GraphQL schema "${remoteSchemaName}" from your GraphQL schema`;
    const isOk = getConfirmation(confirmMessage, true, remoteSchemaName);
    if (isOk) {
      try {
        this.props.dispatch(deleteRemoteSchema());
      } catch (err) {
        console.error(err);
      }
    }
  }

  modifyClick() {
    this.props.dispatch({ type: TOGGLE_MODIFY });
  }

  handleCancelModify() {
    this.props.dispatch({ type: TOGGLE_MODIFY });
  }

  editClicked() {
    this.props.dispatch(modifyRemoteSchema());
  }

  render() {
    const { isFetching, isRequesting, inconsistentObjects } = this.props;
    const { remoteSchemaName } = this.props.params;

    const inconsistencyDetails = inconsistentObjects.find(
      inconObj =>
        inconObj.type === 'remote_schema' &&
        inconObj?.definition?.name === remoteSchemaName
    );

    const fixInconsistencyMsg =
      'This remote schema is in an inconsistent state. Please fix inconsistencies and reload metadata first';

    const generateMigrateBtns = () => {
      return (
        <div className="mt-lg flex">
          <div className="mr-sm">
            <Button
              mode="primary"
              type="submit"
              disabled={isRequesting}
              data-test={'remote-schema-edit-save-btn'}
              isLoading={isRequesting}
              loadingText="Saving..."
            >
              Save
            </Button>
          </div>
          <Button
            mode="destructive"
            size="md"
            onClick={e => {
              this.handleDeleteRemoteSchema(e);
            }}
            disabled={isRequesting || inconsistencyDetails}
            title={inconsistencyDetails ? fixInconsistencyMsg : ''}
            data-test={'remote-schema-edit-delete-btn'}
          >
            {isRequesting ? 'Deleting ...' : 'Delete'}
          </Button>
          {this.state.deleteConfirmationError ? (
            <span
              className={'ml-md text-red-500'}
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
        title: 'Remote schemas',
        url: appPrefix,
      },
      {
        title: 'Manage',
        url: appPrefix + '/' + 'manage',
      },
    ];

    if (remoteSchemaName) {
      breadCrumbs.push({
        title: remoteSchemaName.trim(),
        url:
          appPrefix +
          '/' +
          'manage' +
          '/' +
          remoteSchemaName.trim() +
          '/' +
          'details',
      });
      breadCrumbs.push({
        title: 'modify',
        url: '',
      });
    }

    const titleAnalyticsAttributes = getAnalyticsAttributes(
      'EditRemoteSchema',
      { redactText: true }
    );

    const isEnvVarEnabled = () => {
      return this.props.allRemoteSchemas.some(
        rs => rs.name === remoteSchemaName && rs.definition.url_from_env
      );
    };

    return (
      <div>
        <Helmet>
          <title {...titleAnalyticsAttributes}>
            {`Edit ${pageTitle} - ${remoteSchemaName} - ${pageTitle}s | Hasura`}
          </title>
        </Helmet>
        <Tabs
          appPrefix={appPrefix}
          currentTab="modify"
          heading={remoteSchemaName}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
          showLoader={isFetching}
        />

        {inconsistencyDetails && (
          <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
        )}

        {isFetching ? null : (
          <Analytics name="EditRemoteSchema" {...REDACT_EVERYTHING}>
            <form
              className="bootstrap-jail"
              onSubmit={e => {
                e.preventDefault();
                this.editClicked();
              }}
            >
              <Common {...this.props} isEnvVarEnabled={isEnvVarEnabled()} />
              {generateMigrateBtns()}
            </form>
          </Analytics>
        )}
      </div>
    );
  }
}
const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
    allRemoteSchemas: getRemoteSchemasSelector(state),
    dataHeaders: state.tables.dataHeaders,
    inconsistentObjects: state.metadata.inconsistentObjects,
  };
};

export default connect => connect(mapStateToProps)(Edit);
