import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import JsonInput from '../../../Common/CustomInputTypes/JsonInput';
import TextInput from '../../../Common/CustomInputTypes/TextInput';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/ReusableComponents/ReloadEnumValuesButton';
import { getPlaceholder, BOOLEAN, JSONB, JSONDTYPE, TEXT } from '../utils';
import { ordinalColSort } from '../utils';

// import RichTextEditor from 'react-rte';
import { replace } from 'react-router-redux';
import globals from '../../../../Globals';
import { E_ONGOING_REQ, editItem } from './EditActions';
import { findTable, generateTableDef } from '../../../Common/utils/pgUtils';
import { getTableBrowseRoute } from '../../../Common/utils/routesUtils';

class EditItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, editorColumnMap: {}, currentColumn: null };
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

    const styles = require('../../../Common/TableCommon/Table.scss');

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, currentSchema)
    );

    const columns = currentTable.columns.sort(ordinalColSort);

    const refs = {};

    const elements = columns.map((col, i) => {
      const colName = col.column_name;
      const colType = col.data_type;
      const hasDefault = col.column_default && col.column_default.trim() !== '';
      const isNullable = col.is_nullable && col.is_nullable !== 'NO';
      const isIdentity = col.is_identity && col.is_identity !== 'NO';

      const prevValue = oldItem[colName];

      refs[colName] = {
        valueNode: null,
        valueInput: null,
        nullNode: null,
        defaultNode: null,
      };
      const inputRef = node => (refs[colName].valueInput = node);

      const clicker = e => {
        e.target
          .closest('.radio-inline')
          .querySelector('input[type="radio"]').checked = true;
        e.target.focus();
      };

      const standardEditProps = {
        className: `form-control ${styles.insertBox}`,
        'data-test': `typed-input-${i}`,
        defaultValue: prevValue,
        ref: inputRef,
        type: 'text',
        onClick: clicker,
      };

      const placeHolder = hasDefault
        ? col.column_default
        : getPlaceholder(colType);

      let typedInput = (
        <input {...standardEditProps} placeholder={placeHolder} />
      );

      if (typeof prevValue === 'object') {
        typedInput = (
          <JsonInput
            standardProps={{
              ...standardEditProps,
              defaultValue: JSON.stringify(prevValue),
            }}
            placeholderProp={getPlaceholder(colType)}
          />
        );
      }

      switch (colType) {
        case JSONB:
        case JSONDTYPE:
          typedInput = (
            <JsonInput
              standardProps={{
                ...standardEditProps,
                defaultValue: JSON.stringify(prevValue),
              }}
              placeholderProp={getPlaceholder(colType)}
            />
          );
          break;
        case TEXT:
          typedInput = (
            <TextInput
              standardProps={{ ...standardEditProps }}
              placeholderProp={getPlaceholder(colType)}
            />
          );
          break;
        case BOOLEAN:
          typedInput = (
            <select {...standardEditProps}>
              <option value="" disabled>
                -- bool --
              </option>
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          );
          break;
        default:
          break;
      }

      return (
        <div key={i} className="form-group">
          <label
            className={'col-sm-3 control-label ' + styles.insertBoxLabel}
            title={colName}
          >
            {colName}
          </label>
          <label className={styles.radioLabel + ' radio-inline'}>
            <input
              type="radio"
              ref={node => {
                refs[colName].valueNode = node;
              }}
              name={colName + '-value'}
              value="option1"
            />
            {typedInput}
          </label>
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
        } else if (refs[colName].valueNode.checked) {
          inputValues[colName] =
            refs[colName].valueInput.props !== undefined
              ? refs[colName].valueInput.props.value
              : refs[colName].valueInput.value;
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
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    ...state.tables.update,
    schemas: state.tables.allSchemas,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    currentSchema: state.tables.currentSchema,
  };
};

const editItemConnector = connect => connect(mapStateToProps)(EditItem);

export default editItemConnector;
