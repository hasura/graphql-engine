import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import { ordinalColSort } from '../utils';

// import RichTextEditor from 'react-rte';
import { replace } from 'react-router-redux';
import globals from '../../../../Globals';
import { E_ONGOING_REQ, editItem } from './EditActions';
import { findTable, generateTableDef } from '../../../../dataSources';
import { getTableBrowseRoute } from '../../../Common/utils/routesUtils';
import { fetchEnumOptions } from './EditActions';
import { TableRow } from '../Common/Components/TableRow';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import styles from '../../../Common/TableCommon/Table.scss';

class EditItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, editorColumnMap: {}, currentColumn: null };
  }

  componentDidMount() {
    this.props.dispatch(fetchEnumOptions());
  }

  render() {
    const {
      tableName,
      currentSchema,
      schemas,
      oldItem,
      migrationMode,
      readOnlyMode,
      ongoingRequest,
      lastError,
      lastSuccess,
      count,
      dispatch,
      enumOptions,
      currentSource,
    } = this.props;

    // check if item exists
    if (!oldItem) {
      dispatch(
        replace(
          `${globals.urlPrefix || ''}${getTableBrowseRoute(
            currentSchema,
            currentSource,
            tableName,
            true
          )}`
        )
      );
      return null;
    }

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, currentSchema)
    );

    const columns = currentTable.columns.sort(ordinalColSort);

    const refs = {};

    const elements = columns.map((col, i) => {
      const { column_name: colName } = col;

      const prevValue = oldItem[colName];

      refs[colName] = {
        insertRadioNode: null,
        valueNode: null,
        nullNode: null,
        defaultNode: null,
      };

      return (
        <TableRow
          key={i}
          column={col}
          setRef={(key, node) => (refs[colName][key] = node)}
          enumOptions={enumOptions}
          index={i}
          prevValue={prevValue}
        />
      );
    });

    let alert = null;
    let buttonText = 'Save';
    if (ongoingRequest) {
      alert = (
        <div className="hidden alert alert-warning" role="alert">
          Updating...
        </div>
      );
      buttonText = 'Saving...';
    } else if (lastError) {
      alert = (
        <div className="hidden alert alert-danger" role="alert">
          Error: {JSON.stringify(lastError)}
        </div>
      );
    } else if (lastSuccess) {
      alert = (
        <div className="hidden alert alert-success" role="alert">
          Updated! <br /> {JSON.stringify(lastSuccess)}
        </div>
      );
    }

    const handleSaveClick = e => {
      e.preventDefault();

      const inputValues = {};
      Object.keys(refs).map(colName => {
        if (refs[colName].nullNode.checked) {
          // null
          inputValues[colName] = null;
        } else if (refs[colName].defaultNode.checked) {
          // default
          inputValues[colName] = { default: true };
        } else if (refs[colName].insertRadioNode.checked) {
          inputValues[colName] =
            refs[colName].valueNode.props !== undefined
              ? refs[colName].valueNode.props.value
              : refs[colName].valueNode.value;
        }
      });

      dispatch({ type: E_ONGOING_REQ });

      dispatch(editItem(tableName, inputValues));
    };

    return (
      <RightContainer>
        <div className={styles.container + ' container-fluid'}>
          <TableHeader
            count={count}
            dispatch={dispatch}
            table={currentTable}
            source={currentSource}
            tabName="edit"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
          />
          <br />
          <div className={styles.insertContainer + ' container-fluid'}>
            <div className="col-xs-9">
              <form id="updateForm" className="form-horizontal">
                {elements}
                <Button
                  type="submit"
                  color="yellow"
                  size="sm"
                  onClick={handleSaveClick}
                  data-test="edit-save-button"
                >
                  {buttonText}
                </Button>
                {currentTable.is_enum ? (
                  <ReloadEnumValuesButton dispatch={dispatch} />
                ) : null}
              </form>
            </div>
            <div className="col-xs-3">{alert}</div>
          </div>
          <br />
          <br />
        </div>
      </RightContainer>
    );
  }
}

EditItem.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  schemas: PropTypes.array.isRequired,
  oldItem: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastSuccess: PropTypes.object,
  lastError: PropTypes.object,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
  count: PropTypes.number,
  dispatch: PropTypes.func.isRequired,
  enumOptions: PropTypes.object,
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    ...state.tables.update,
    schemas: state.tables.allSchemas,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    currentSchema: state.tables.currentSchema,
    currentSource: state.tables.currentDataSource,
  };
};

const editItemConnector = connect => connect(mapStateToProps)(EditItem);

export default editItemConnector;
