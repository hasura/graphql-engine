import React from 'react';
import Button from '../../../Common/Button/Button';
import { dropInconsistentObjects, loadInconsistentObjects } from './Actions';
import { permissionTypes } from './metadataFilters';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';
import metaDataStyles from './Metadata.scss';
import styles from '../../../Common/TableCommon/Table.scss';
import CheckIcon from '../../../Common/Icons/Check';
import CrossIcon from '../../../Common/Icons/Cross';

const MetadataStatus = ({
  dispatch,
  supportInconsistentMetadata,
  metadata,
}) => {
  if (!supportInconsistentMetadata) {
    return null;
  }

  const inconsistentObjectsTable = () => {
    return (
      <table
        className={`${metaDataStyles.metadataStatusTable} ${
          metaDataStyles.wd750
        }`}
        id="t01"
      >
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Definition</th>
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
              definition = `relationship of table "${ico.definition.table}"`;
            } else if (permissionTypes.includes(ico.type)) {
              name = `${ico.definition.role}-permission`;
              definition = `${ico.type} on table "${ico.definition.table}"`;
            } else if (ico.type === 'table') {
              name = ico.definition;
              definition = ico.definition;
            } else if (ico.type === 'function') {
              name = ico.definition.name;
              definition = ico.name;
            } else if (ico.type === 'event_trigger') {
              name = ico.definition.configuration.name;
              definition = `event triggeer on table "${
                ico.definition.configuration.table
              }"`;
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
    const isOk = window.confirm(
      'Are you sure? This will drop all the inconsistent parts of your metadata. This action is irreversible.'
    );
    if (isOk) {
      dispatch(dropInconsistentObjects());
    }
  };

  const reloadCacheAndLoadInconsistentObjects = () => {
    dispatch(loadInconsistentObjects(null, true))
      .then(() => {
        dispatch(showSuccessNotification('Metadata reloaded'));
      })
      .catch(e => {
        // todo error handling
        console.error(e);
        dispatch(showErrorNotification('Error reloading metadata'));
      });
  };

  const content = () => {
    if (metadata.inconsistentObjects.length === 0) {
      return (
        <div className={metaDataStyles.content_width}>
          <CheckIcon className={metaDataStyles.add_mar_right_small} />
          GraphQL Engine metadata is consistent with Postgres
        </div>
      );
    }
    return (
      <div>
        <div className={metaDataStyles.content_width}>
          <CrossIcon className={metaDataStyles.add_mar_right_small} />
          GraphQL Engine metadata is inconsistent with Postgres
        </div>
        <div>{inconsistentObjectsTable()}</div>
        <div
          className={`${metaDataStyles.add_mar_top_small} ${
            metaDataStyles.content_width
          }`}
        >
          <ul>
            <li>
              To delete all the inconsistent objects, click the "Delete all"
              button
            </li>
            <li>
              If you want to manage these objects on your own, please do so and
              click on the "Reload Metadata" button to check if the
              inconsistencies have been resolved
            </li>
          </ul>
        </div>
        <div className={metaDataStyles.display_flex}>
          <Button
            color="yellow"
            size="sm"
            className={`${metaDataStyles.add_mar_top_small} ${
              metaDataStyles.add_mar_right
            }`}
            onClick={reloadCacheAndLoadInconsistentObjects}
          >
            Reload metadata
          </Button>
          <Button
            color="red"
            size="sm"
            className={metaDataStyles.add_mar_top_small}
            onClick={verifyAndDropAll}
          >
            Delete all
          </Button>
        </div>
      </div>
    );
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${
        metaDataStyles.metadata_wrapper
      } container-fluid`}
    >
      <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
        Metadata Status
      </h2>
      {content()}
    </div>
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
