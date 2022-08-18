import React from 'react';
import { push } from 'react-router-redux';
import globals from '../../../../Globals';
import { getRemoteSchemasSelector } from '../../../../metadata/selector';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import WarningSymbol from '../../../Common/WarningSymbol/WarningSymbol';
import ReloadRemoteSchema from '../../Settings/MetadataOptions/ReloadRemoteSchema';
import { VIEW_REMOTE_SCHEMA } from '../Actions';
import {
  fetchRemoteSchema,
  getHeaderEvents,
  RESET,
} from '../Add/addRemoteSchemaReducer';
import { Tabs } from '../Common/Tabs';
import { appPrefix } from '../constants';

const prefixUrl = globals.urlPrefix + appPrefix;

const RSHeadersDisplay = ({ data }) =>
  data.length > 0 ? (
    <div className="w-4/5 flex border-t-0 border border-gray-300 bg-gray-50">
      <div className="w-1/3 p-sm border-r border-gray-300">Headers</div>
      <div className="w-2/3 p-sm">
        {data &&
          data
            .filter(header => !!header.name)
            .map((header, index) => [
              <div className="flex justify-start items-start">
                {`${header.name}: `}
                <div>
                  {header.type === 'static'
                    ? header.value
                    : '<' + header.value + '>'}
                </div>
              </div>,
              index !== data.length - 1 ? <hr className="my-sm" /> : null,
            ])}
      </div>
    </div>
  ) : null;

const RSReloadSchema = ({ readOnlyMode, remoteSchemaName, ...props }) =>
  !readOnlyMode && remoteSchemaName && remoteSchemaName.length > 0 ? (
    <div className="pt-sm">
      <ReloadRemoteSchema {...props} remoteSchemaName={remoteSchemaName} />
      <ToolTip
        placement="right"
        message="If your remote schema has changed, you need to refresh the GraphQL Engine metadata to query the modified schema"
      />
    </div>
  ) : null;

class ViewStitchedSchema extends React.Component {
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

  render() {
    const { remoteSchemaName } = this.props.params;
    const { manualUrl, envName, headers, readOnlyMode, inconsistentObjects } =
      this.props;

    const filterHeaders = headers.filter(h => !!h.name);

    const breadCrumbs = [
      {
        title: 'Remote schemas',
        url: appPrefix,
      },
      {
        title: 'Manage',
        url: `${appPrefix}/manage`,
      },
    ];

    if (remoteSchemaName) {
      breadCrumbs.push({
        title: remoteSchemaName.trim(),
        url: `${appPrefix}/manage/${remoteSchemaName.trim()}/details`,
      });
      breadCrumbs.push({
        title: 'details',
        url: '',
      });
    }

    const inconsistencyDetails = inconsistentObjects.find(
      inconObj =>
        inconObj.type === 'remote_schema' &&
        inconObj?.definition?.name === remoteSchemaName
    );

    return (
      <div className={''}>
        <Tabs
          appPrefix={appPrefix}
          currentTab="details"
          heading={remoteSchemaName}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage/${remoteSchemaName}`}
        />
        <br />
        <div>
          <div>
            <div className="mt-md w-4/5 flex border border-gray-300 bg-gray-50">
              <div className="w-1/3 p-xs border-r border-gray-300">
                GraphQL Server URL
              </div>
              <div className="w-2/3 p-xs">{manualUrl || `<${envName}>`}</div>
            </div>
            <RSHeadersDisplay data={filterHeaders} />
          </div>
          {inconsistencyDetails && (
            <div className="md-sm">
              <div className="text-lg font-bold">
                <WarningSymbol tooltipText={'Inconsistent schema'} />
                <span className="ml-sm">
                  This remote schema is in an inconsistent state.
                </span>
              </div>
              <div>
                <b>Reason:</b> {inconsistencyDetails.reason}
              </div>
              <div>
                <i>
                  (Please resolve the inconsistencies and reload the remote
                  schema. Fields from this remote schema are currently not
                  exposed over the GraphQL API)
                </i>
              </div>
            </div>
          )}
          <RSReloadSchema
            readOnlyMode={readOnlyMode}
            remoteSchemaName={remoteSchemaName}
            {...this.props}
          />
        </div>
        <br />
        <br />
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
    readOnlyMode: state.main.readOnlyMode,
    inconsistentObjects: state.metadata.inconsistentObjects,
  };
};

export default connect => connect(mapStateToProps)(ViewStitchedSchema);
