import React, { useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { permissionTypes, getTableNameFromDef } from '../utils';
import CheckIcon from '../../../Common/Icons/Check';
import CrossIcon from '../../../Common/Icons/Cross';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import ReloadMetadata from '../MetadataOptions/ReloadMetadata';
import { dropInconsistentObjects } from '../../../../metadata/actions';
import {
  fetchDataInit,
  SET_INCONSISTENT_INHERITED_ROLE,
  updateCurrentSchema,
  UPDATE_CURRENT_DATA_SOURCE,
} from '../../Data/DataActions';
import { setDriver } from '../../../../dataSources';
import _push from '../../Data/push';
import { FaExclamationCircle, FaTimes } from 'react-icons/fa';

const MetadataStatus = ({ dispatch, metadata }) => {
  const [shouldShowErrorBanner, toggleErrorBanner] = useState(true);
  const [isLoading, setIsLoading] = useState(false);
  const dismissErrorBanner = () => {
    toggleErrorBanner(false);
  };

  const resolveInconsistentInheritedRole = inconsistentInheritedRoleObj => {
    const { table, source, permission_type } =
      inconsistentInheritedRoleObj.entity;
    const role = inconsistentInheritedRoleObj.name;
    const schema = metadata.metadataObject.sources
      .find(s => s.name === source)
      .tables.find(t => t.table.name === table).table.schema;

    const driver = metadata.metadataObject.sources.find(
      s => s.name === source
    ).kind;

    /*
      Load up the database details and schema details
    */

    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: source,
    });
    setDriver(driver);

    dispatch(fetchDataInit(source, driver)).finally(() => {
      dispatch(updateCurrentSchema(schema, source)).then(() => {
        dispatch({
          type: SET_INCONSISTENT_INHERITED_ROLE,
          inconsistent_inherited_role: {
            role: role,
            permission_type: permission_type,
            table: table,
            schema: schema,
            source: source,
          },
        });
        dispatch(
          _push(`/data/${source}/schema/${schema}/tables/${table}/permissions`)
        );
      });
    });
  };

  const inconsistentObjectsTable = () => {
    return (
      <table className={`w-1/2 mt-lg`} id="t01">
        <thead>
          <tr>
            <th className="p-xs pr-md border">Name</th>
            <th className="p-xs pr-md border">Type</th>
            <th className="p-xs pr-md border">Description</th>
            <th className="p-xs pr-md border">Reason</th>
            <th className="p-xs pr-md border">Action</th>
          </tr>
        </thead>
        <tbody>
          {[
            ...metadata.inconsistentObjects,
            ...metadata.inconsistentInheritedRoles,
          ].map((inconsistentObject, _i) => {
            let name;
            let definition;
            if (inconsistentObject.type === 'source') {
              name = inconsistentObject.definition;
            }
            if (
              inconsistentObject.type === 'object_relation' ||
              inconsistentObject.type === 'array_relation'
            ) {
              name = inconsistentObject.definition.name;
              definition = `relationship of table "${getTableNameFromDef(
                inconsistentObject.definition.table
              )}"`;
            } else if (inconsistentObject.type === 'remote_relationship') {
              name = inconsistentObject.definition.name;
              definition = `relationship between table "${getTableNameFromDef(
                inconsistentObject.definition.table
              )}" and remote schema "${
                inconsistentObject.definition.remote_schema
              }"`;
            } else if (permissionTypes.includes(inconsistentObject.type)) {
              name = `${inconsistentObject.definition.role}-permission`;
              definition = `${
                inconsistentObject.type
              } on table "${getTableNameFromDef(
                inconsistentObject.definition.table
              )}"`;
            } else if (inconsistentObject.type === 'table') {
              name = getTableNameFromDef(inconsistentObject.definition);
              definition = name;
            } else if (inconsistentObject.type === 'function') {
              name = getTableNameFromDef(inconsistentObject.definition);
              definition = name;
            } else if (inconsistentObject.type === 'event_trigger') {
              name = inconsistentObject.definition.configuration.name;
              definition = `event trigger on table "${getTableNameFromDef(
                inconsistentObject.definition.table
              )}"`;
            } else if (inconsistentObject.type === 'remote_schema') {
              name = inconsistentObject.definition.name;
              let url = `"${
                inconsistentObject.definition.definition.url ||
                inconsistentObject.definition.definition.url_from_env
              }"`;
              if (inconsistentObject.definition.definition.url_from_env) {
                url = `the url from the value of env var ${url}`;
              }
              definition = `remote schema named "${name}" at ${url}`;
            }
            const message = inconsistentObject.message;
            return (
              <tr key={_i}>
                <td
                  className="p-xs pr-md border"
                  data-test={`inconsistent_name_${_i}`}
                >
                  {name}
                </td>
                <td
                  className="p-xs pr-md border"
                  data-test={`inconsistent_type_${_i}`}
                >
                  {`${inconsistentObject.type} `}
                </td>
                <td
                  className="p-xs pr-md border"
                  data-test={`inconsistent_description_${_i}`}
                >
                  {definition}
                </td>
                <td
                  className="p-xs pr-md border"
                  data-test={`inconsistent_reason_${_i}`}
                >
                  <div>
                    <b>{inconsistentObject.reason}</b>
                    <br />
                    {typeof message === 'string' ? (
                      message
                    ) : message ? (
                      <pre className="text-base">
                        <code>{JSON.stringify(message, null, 2)}</code>
                      </pre>
                    ) : null}
                  </div>
                </td>
                <td
                  className="p-xs pr-md border"
                  data-test={`inconsistent_action_${_i}`}
                >
                  {inconsistentObject.type ===
                  'inherited role permission inconsistency' ? (
                    <Button
                      onClick={() =>
                        resolveInconsistentInheritedRole(inconsistentObject)
                      }
                    >
                      Resolve
                    </Button>
                  ) : null}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    );
  };

  const verifyAndDropAll = () => {
    const confirmMessage = `This will drop all the inconsistent objects in your metadata. This includes all inconsistent sources - databases, remote schemas, actions etc. and any Hasura features related to these objects. This action is irreversible.`;
    const isOk = getConfirmation(confirmMessage);
    const callback = () => setIsLoading(false);
    if (isOk) {
      setIsLoading(true);
      dispatch(dropInconsistentObjects(callback, callback));
    }
  };

  const content = () => {
    const isInconsistentRemoteSchemaPresent = metadata.inconsistentObjects.some(
      i => i.type === 'remote_schema'
    );
    if (
      metadata.inconsistentObjects.length === 0 &&
      metadata.inconsistentInheritedRoles.length === 0
    ) {
      return (
        <div className="mt-md">
          <div className="w-8/12">
            <div className="flex">
              <CheckIcon className="mr-sm" />
              <h4 className="text-lg font-bold">
                GraphQL Engine metadata is consistent with database
              </h4>
            </div>
          </div>
        </div>
      );
    }

    return (
      <div className="mt-md">
        <div className="w-8/12">
          <div className="flex">
            <CrossIcon className="mr-sm" />
            <h4 className="text-lg font-bold">
              {' '}
              GraphQL Engine metadata is inconsistent with database{' '}
            </h4>
          </div>
          <div className="mt-md">
            <div className="mt-xs">
              The following objects in your metadata are inconsistent because
              they reference database or remote-schema entities which do not
              seem to exist or are conflicting
            </div>
            <div className="mt-xs">
              The GraphQL API has been generated using only the consistent parts
              of the metadata
            </div>
            <div className="mt-xs">
              The console might also not be able to display these inconsistent
              objects
            </div>
          </div>
        </div>
        <div>{inconsistentObjectsTable()}</div>
        <div className={`w-1/2 mt-md`}>
          To resolve these inconsistencies, you can do one of the following:
          <ul className="mt-xs">
            <li>
              To delete all the inconsistent objects from the metadata, click
              the "Delete all" button
            </li>
            <li>
              If you want to manage these objects on your own, please do so and
              click on the "Reload Metadata" button to check if the
              inconsistencies have been resolved
            </li>
          </ul>
        </div>
        <div className={`flex mt-sm`}>
          <Button
            mode="destructive"
            size="sm"
            className="mr-md"
            onClick={verifyAndDropAll}
            isLoading={isLoading}
            loadingText={'Deleting...'}
          >
            Delete all
          </Button>
          <ReloadMetadata
            dispatch={dispatch}
            buttonText="Reload metadata"
            shouldReloadRemoteSchemas={isInconsistentRemoteSchemaPresent}
          />
        </div>
      </div>
    );
  };

  const banner = () => {
    if (
      metadata.inconsistentObjects.length === 0 &&
      metadata.inconsistentInheritedRoles.length === 0
    ) {
      return null;
    }
    if (!shouldShowErrorBanner) {
      return null;
    }
    const urlSearchParams = new URLSearchParams(window.location.search);
    if (
      urlSearchParams.get('is_redirected') !== 'true' &&
      metadata.inconsistentObjects.length !== 0
    ) {
      return null;
    }
    return (
      <div
        className={`w-full p-md bg-red-100 items-center flex justify-between flex-row border border-red-200`}
      >
        <div>
          <FaExclamationCircle
            className={`font-normal text-red-800 mr-sm`}
            aria-hidden="true"
          />
          <strong className="text-red-800">
            You have been redirected because your GraphQL Engine metadata is in
            an inconsistent state
          </strong>
        </div>
        <FaTimes
          className={`text-normal font-normal cursor-pointer text-red-800`}
          aria-hidden="true"
          onClick={dismissErrorBanner}
        />
      </div>
    );
  };

  return (
    <Analytics name="MetadataStatus" {...REDACT_EVERYTHING}>
      <div className="mb-md bootstrap-jail">
        {banner()}
        <div className="clear-both pl-md mt-md mb-md">
          <h2 className="text-xl font-bold">Hasura Metadata Status</h2>
          {content()}
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const connector = connect => connect(mapStateToProps)(MetadataStatus);

export default connector;
