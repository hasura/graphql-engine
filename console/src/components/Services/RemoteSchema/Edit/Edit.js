import React from 'react';
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
import tabInfo from './tabInfo';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import Button from '../../../Common/Button/Button';

import { appPrefix, pageTitle } from '../constants';

import { NotFoundError } from '../../../Error/PageNotFound';

import globals from '../../../../Globals';

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

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.params.remoteSchemaName !== this.props.params.remoteSchemaName
    ) {
      Promise.all([
        this.props.dispatch(
          fetchRemoteSchema(nextProps.params.remoteSchemaName)
        ),
        this.props.dispatch({
          type: VIEW_REMOTE_SCHEMA,
          data: nextProps.params.remoteSchemaName,
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

    const a = prompt(
      'Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n'
    );

    try {
      if (a && typeof a === 'string' && a.trim() === 'DELETE') {
        this.updateDeleteConfirmationError(null);
        this.props.dispatch(deleteRemoteSchema());
      } else {
        // Input didn't match
        // Show an error message right next to the button
        this.updateDeleteConfirmationError('user confirmation error!');
      }
    } catch (err) {
      console.error(err);
    }
  }

  updateDeleteConfirmationError(data) {
    this.setState({ deleteConfirmationError: data });
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
    const currentRemoteSchema = this.props.allRemoteSchemas.find(
      r => r.name === this.props.params.remoteSchemaName
    );

    if (!currentRemoteSchema) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const styles = require('../RemoteSchema.scss');

    const { isFetching, isRequesting, editState } = this.props;
    const { remoteSchemaName } = this.props.params;

    const generateMigrateBtns = () => {
      return 'isModify' in editState && !editState.isModify ? (
        <div className={styles.commonBtn}>
          <Button
            className={styles.button_mar_right}
            color="yellow"
            size="sm"
            onClick={e => {
              e.preventDefault();
              this.modifyClick();
            }}
            data-test={'remote-schema-edit-modify-btn'}
            disabled={isRequesting}
          >
            Modify
          </Button>
          <Button
            color="red"
            size="sm"
            onClick={e => {
              e.preventDefault();
              this.handleDeleteRemoteSchema(e);
            }}
            disabled={isRequesting}
            data-test={'remote-schema-edit-delete-btn'}
          >
            {isRequesting ? 'Deleting ...' : 'Delete'}
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
      ) : (
        <div className={styles.commonBtn}>
          <Button
            className={styles.button_mar_right}
            color="yellow"
            size="sm"
            type="submit"
            disabled={isRequesting}
            data-test={'remote-schema-edit-save-btn'}
          >
            {isRequesting ? 'Saving' : 'Save'}
          </Button>
          <Button
            color="white"
            size="sm"
            onClick={e => {
              e.preventDefault();
              this.handleCancelModify();
            }}
            data-test={'remote-schema-edit-cancel-btn'}
            disabled={isRequesting}
          >
            Cancel
          </Button>
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

    return (
      <div className={styles.addWrapper}>
        <Helmet
          title={`Edit ${pageTitle} - ${remoteSchemaName} - ${pageTitle}s | Hasura`}
        />
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab="modify"
          heading={remoteSchemaName}
          tabsInfo={tabInfo}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
          showLoader={isFetching}
        />
        {isFetching ? null : (
          <form
            onSubmit={e => {
              e.preventDefault();
              this.editClicked();
            }}
          >
            <Common {...this.props} />
            {generateMigrateBtns()}
          </form>
        )}
      </div>
    );
  }
}
const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.addData,
    ...state.remoteSchemas.headerData,
    allRemoteSchemas: state.remoteSchemas.listData.remoteSchemas,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

export default connect => connect(mapStateToProps)(Edit);
