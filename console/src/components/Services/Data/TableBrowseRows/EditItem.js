import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import { ordinalColSort, getForeignKey } from '../utils';

// import RichTextEditor from 'react-rte';
import { replace } from 'react-router-redux';
import globals from '../../../../Globals';
import { E_ONGOING_REQ, editItem } from './EditActions';
import { findTable, generateTableDef } from '../../../Common/utils/pgUtils';
import { getTableBrowseRoute } from '../../../Common/utils/routesUtils';
import { fetchEnumOptions } from './EditActions';
import { filterFkOptions } from '../DataActions';
import styles from '../../../Common/TableCommon/Table.scss';

import { getExistingFKConstraints } from '../Common/Components/utils';
import { TypedInput } from '../Common/Components/TypedInput';

class EditItem extends Component {
  constructor() {
    super();
    this.state = {
      insertedRows: 0,
      editorColumnMap: {},
      currentColumn: null,
      selectedFkOptions: {},
    };
  }

  componentDidMount() {
    this.props.dispatch(fetchEnumOptions());
  }
  handleSearchValueChange = (config, value) => {
    this.props.dispatch(filterFkOptions(config, value));
  };
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
      fkOptions,
    } = this.props;

    // check if item exists
    if (!oldItem) {
      dispatch(
        replace(
          `${globals.urlPrefix || ''}${getTableBrowseRoute(
            currentSchema,
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
    // columns in the right order with their indices
    const orderedColumns = columns.map((c, i) => ({
      name: c.column_name,
      index: i,
    }));
    // restructure the existing foreign keys and add it to fkModify (for easy processing)
    const existingForeignKeys = getExistingFKConstraints(
      currentTable,
      orderedColumns
    );

    // Generate a list of reference schemas and their columns
    const refTables = {};
    existingForeignKeys.map(fk => {
      schemas.forEach(ts => {
        if (ts.table_schema === fk.refSchemaName) {
          refTables[ts.table_name] = ts.columns.map(c => ({
            name: c.column_name,
            type: c.data_type,
          }));
        }
      });
    });
    const refs = {};

    const elements = columns.map((col, i) => {
      const { column_name: colName } = col;
      const hasDefault = col.column_default && col.column_default.trim() !== '';
      const isNullable = col.is_nullable && col.is_nullable !== 'NO';
      const isIdentity = col.is_identity && col.is_identity !== 'NO';

      const prevValue = oldItem[colName];

      refs[colName] = {
        insertRadioNode: null,
        valueNode: null,
        nullNode: null,
        defaultNode: null,
      };

      const handleFkOptionChange = ({ value, label }) => {
        this.setState(prev => ({
          ...prev,
          selectedFkOptions: {
            ...prev.selectedFkOptions,
            [colName]: { value, label },
          },
        }));
      };

      return (
        <div key={i} className={`form-group ${styles.displayFlexContainer}`}>
          <label
            className={'col-sm-3 control-label ' + styles.insertBoxLabel}
            title={colName}
          >
            {colName}
          </label>
          <span
            className={`${styles.radioLabel} ${styles.typedInputWrapper} radio-inline`}
          >
            <input
              type="radio"
              ref={node => {
                refs[colName].valueNode = node;
              }}
              name={colName + '-value'}
              value="option1"
            />
            <TypedInput
              inputRef={node => {
                refs[colName].valueInput = node;
              }}
              prevValue={prevValue}
              enumOptions={enumOptions}
              col={col}
              index={i}
              hasDefault={hasDefault}
              fkOptions={fkOptions}
              getFkOptions={this.handleSearchValueChange}
              selectedOption={this.state.selectedFkOptions[colName]}
              onFkValueChange={handleFkOptionChange}
              refTables={refTables}
              foreignKey={getForeignKey(col, schemas)}
            >
              <>
                <label className={styles.radioLabel + ' radio-inline'}>
                  <input
                    type="radio"
                    ref={node => {
                      refs[colName].nullNode = node;
                    }}
                    disabled={!isNullable}
                    name={colName + '-value'}
                    value="NULL"
                    defaultChecked={prevValue === null}
                  />
                  <span className={styles.radioSpan}>NULL</span>
                </label>
                <label className={styles.radioLabel + ' radio-inline'}>
                  <input
                    type="radio"
                    ref={node => {
                      refs[colName].defaultNode = node;
                    }}
                    name={colName + '-value'}
                    value="option3"
                    disabled={!hasDefault && !isIdentity}
                    defaultChecked={isIdentity}
                  />
                  <span className={styles.radioSpan}>Default</span>
                </label>
              </>
            </TypedInput>
          </span>
        </div>
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
        } else if (this.state.selectedFkOptions[colName]) {
          console.log({ state: this.state });
          inputValues[colName] = this.state.selectedFkOptions[colName].value;
        } else if (refs[colName].valueNode.checked) {
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
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          table={currentTable}
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
              <ReloadEnumValuesButton
                dispatch={dispatch}
                isEnum={currentTable.is_enum}
              />
            </form>
          </div>
          <div className="col-xs-3">{alert}</div>
        </div>
        <br />
        <br />
      </div>
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
    fkOptions: state.tables.fkOptions,
  };
};

const editItemConnector = connect => connect(mapStateToProps)(EditItem);

export default editItemConnector;
