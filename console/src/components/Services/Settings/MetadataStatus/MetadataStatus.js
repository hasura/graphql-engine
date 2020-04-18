import React, { useState } from 'react';

import Button from '../../../Common/Button/Button';
import { dropInconsistentObjects } from '../Actions';
import { permissionTypes, getTableNameFromDef } from '../utils';
import metaDataStyles from '../Settings.scss';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import ReloadMetadata from '../MetadataOptions/ReloadMetadata';
import { Icon, Heading, Text, Box, Flex } from '../../../UIKit/atoms';
import styles from '../../../Common/TableCommon/Table.scss';

const MetadataStatus = ({ dispatch, metadata }) => {
  const [shouldShowErrorBanner, toggleErrorBanner] = useState(true);
  const [isLoading, setIsLoading] = useState(false);
  const dismissErrorBanner = () => {
    toggleErrorBanner(false);
  };
  const inconsistentObjectsTable = () => {
    return (
      <table
        className={`${metaDataStyles.metadataStatusTable} ${metaDataStyles.wd750}`}
        id="t01"
      >
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Description</th>
            <th>Reason</th>
          </tr>
        </thead>
        <tbody>
          {metadata.inconsistentObjects.map((ico, _i) => {
            let name;
            let definition;
            if (
              ico.type === 'object_relation' ||
              ico.type === 'array_relation'
            ) {
              name = ico.definition.name;
              definition = `relationship of table "${getTableNameFromDef(
                ico.definition.table
              )}"`;
            } else if (permissionTypes.includes(ico.type)) {
              name = `${ico.definition.role}-permission`;
              definition = `${ico.type} on table "${getTableNameFromDef(
                ico.definition.table
              )}"`;
            } else if (ico.type === 'table') {
              name = getTableNameFromDef(ico.definition);
              definition = name;
            } else if (ico.type === 'function') {
              name = getTableNameFromDef(ico.definition);
              definition = name;
            } else if (ico.type === 'event_trigger') {
              name = ico.definition.configuration.name;
              definition = `event trigger on table "${getTableNameFromDef(
                ico.definition.table
              )}"`;
            } else if (ico.type === 'remote_schema') {
              name = ico.definition.name;
              let url = `"${ico.definition.definition.url ||
                ico.definition.definition.url_from_env}"`;
              if (ico.definition.definition.url_from_env) {
                url = `the url from the value of env var ${url}`;
              }
              definition = `remote schema named "${name}" at ${url}`;
            }
            return (
              <tr key={_i}>
                <td>{name}</td>
                <td>{ico.type}</td>
                <td>{definition}</td>
                <td>{ico.reason}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    );
  };

  const verifyAndDropAll = () => {
    const confirmMessage =
      'This will drop all the inconsistent objects in your metadata. This action is irreversible.';
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
    if (metadata.inconsistentObjects.length === 0) {
      return (
        <Flex mt="20px">
          <Icon type="check" color="green.original" mr="sm" />
          <Heading as="h4">
            GraphQL Engine metadata is consistent with database
          </Heading>
        </Flex>
      );
    }

    return (
      <Flex mt="20px">
        <Icon type="close" color="red.primary" mr="sm" />
        <Heading as="h4">
          GraphQL Engine metadata is inconsistent with database
        </Heading>
        <Box mt="20px">
          <Box my="5px">
            The following objects in your metadata are inconsistent because they
            reference database or remote-schema entities which do not seem to
            exist or are conflicting
          </Box>
          <Box>
            The GraphQL API has been generated using only the consistent parts
            of the metadata
          </Box>
          <Box mt="5px">
            The console will also not be able to display these inconsistent
            objects
          </Box>
        </Box>
        <div className={styles.add}>{inconsistentObjectsTable()}</div>
        <Box mt="20px" width="50%">
          To resolve these inconsistencies, you can do one of the following:
          <ul className={styles.add_mar_top_small}>
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
        </Box>
        <Flex mt="5px">
          <Button
            color="red"
            size="sm"
            className={metaDataStyles.add_mar_right}
            onClick={verifyAndDropAll}
            disabled={isLoading}
          >
            Delete all
          </Button>
          <ReloadMetadata
            dispatch={dispatch}
            buttonText="Reload metadata"
            shouldReloadRemoteSchemas={isInconsistentRemoteSchemaPresent}
          />
        </Flex>
      </Flex>
    );
  };

  const banner = () => {
    if (metadata.inconsistentObjects.length === 0) {
      return null;
    }
    if (!shouldShowErrorBanner) {
      return null;
    }
    const urlSearchParams = new URLSearchParams(window.location.search);
    if (urlSearchParams.get('is_redirected') !== 'true') {
      return null;
    }

    return (
      <Flex
        top={0}
        zIndex={1000}
        className="alert alert-danger"
        justifyContent="space-between"
      >
        <Text fontWeight="bold">
          <Icon type="error" mr="sm" mb="-2px" />
          You have been redirected because your GraphQL Engine metadata is in an
          inconsistent state
        </Text>
        <Icon type="close" onClick={dismissErrorBanner} pointer />
      </Flex>
    );
  };

  return (
    <Box mb="20px">
      {banner()}
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${metaDataStyles.metadata_wrapper} container-fluid`}
      >
        <Heading as="h2" pb="0px" fontSize="18px">
          Hasura Metadata Status
        </Heading>
        {content()}
      </div>
    </Box>
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
