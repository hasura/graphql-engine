import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { editItem, E_ONGOING_REQ } from './EditActions';
import globals from '../../../../Globals';
import { modalClose } from './EditActions';
import Button from '../../../Common/Button/Button';

import {
  getPlaceholder,
  INTEGER,
  BIGINT,
  NUMERIC,
  DATE,
  BOOLEAN,
  UUID,
  JSONDTYPE,
  JSONB,
  TIMESTAMP,
  TIMETZ,
} from '../../../../constants';
// import RichTextEditor from 'react-rte';
import { replace } from 'react-router-redux';

class EditItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, editorColumnMap: {}, currentColumn: null };
  }

  onTextChange = (e, colName) => {
    this.setState({
      editorColumnMap: {
        ...this.state.editorColumnMap,
        [colName]: e.target.value,
      },
    });
  };

  onModalClose = () => {
    this.props.dispatch(modalClose());
  };

  render() {
    const {
      tableName,
      currentSchema,
      schemas,
      oldItem,
      migrationMode,
      ongoingRequest,
      lastError,
      lastSuccess,
      dispatch,
    } = this.props;

    // check if item exists
    if (!oldItem) {
      dispatch(
        replace(
          `${globals.urlPrefix ||
            ''}/data/schema/${currentSchema}/tables/${tableName}/browse`
        )
      );
      return null;
    }

    const styles = require('../TableCommon/Table.scss');
    const columns = schemas.find(x => x.table_name === tableName).columns;

    const refs = {};
    const elements = columns.map((col, i) => {
      const colName = col.column_name;
      const colType = col.data_type;
      refs[colName] = { valueNode: null, nullNode: null, defaultNode: null };
      const inputRef = node => {
        refs[colName].valueNode = node;
      };
      const clicker = e => {
        e.target.parentNode.click();
        e.target.focus();
      };

      // Text type
      let typedInput = (
        <input
          placeholder={getPlaceholder(colType)}
          type="text"
          className={'form-control ' + styles.insertBox}
          onClick={clicker}
          ref={inputRef}
          defaultValue={oldItem[colName]}
          data-test={`typed-input-${i}`}
        />
      );

      // Integer
      if (colType === INTEGER) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === BIGINT) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === NUMERIC) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === TIMESTAMP) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === DATE) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === TIMETZ) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === JSONDTYPE || colType === JSONB) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={JSON.stringify(oldItem[colName])}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === BOOLEAN) {
        typedInput = (
          <select
            className="form-control"
            onClick={clicker}
            ref={inputRef}
            defaultValue={JSON.stringify(oldItem[colName])}
            onClick={e => {
              e.target.parentNode.parentNode.click();
              e.target.focus();
            }}
            data-test={`typed-input-${i}`}
          >
            <option value="true">True</option>
            <option value="false">False</option>
          </select>
        );
      } else if (colType === UUID) {
        typedInput = (
          <input
            placeholder={getPlaceholder(colType)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else {
        // everything else is text.
        // find value to be shown. rich text editor vs clone
        let defaultValue = '';
        let currentValue = '';
        if (
          this.state.editorColumnMap[colName] === null ||
          this.state.editorColumnMap[colName] === undefined
        ) {
          defaultValue = oldItem[colName];
        } else if (this.state.editorColumnMap[colName] !== null) {
          defaultValue = this.state.editorColumnMap[colName];
          currentValue = this.state.editorColumnMap[colName];
        }
        if (currentValue !== '') {
          typedInput = (
            <span>
              <input
                placeholder={'text'}
                type="text"
                className={'form-control ' + styles.insertBox}
                onClick={clicker}
                ref={inputRef}
                onChange={e => {
                  this.onTextChange(e, colName);
                }}
                value={currentValue}
                data-test={`typed-input-${i}`}
              />
            </span>
          );
        } else {
          typedInput = (
            <span>
              <input
                placeholder={'text'}
                type="text"
                className={'form-control ' + styles.insertBox}
                onClick={clicker}
                ref={inputRef}
                onChange={e => {
                  this.onTextChange(e, colName);
                }}
                value={defaultValue}
                data-test={`typed-input-${i}`}
              />
            </span>
          );
        }
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
            <input type="radio" name={colName + '-value'} value="option1" />
            {typedInput}
          </label>
          <label className={styles.radioLabel + ' radio-inline'}>
            <input
              type="radio"
              ref={node => {
                refs[colName].nullNode = node;
              }}
              name={colName + '-value'}
              value="NULL"
              defaultChecked={oldItem[colName] === null ? true : false}
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
    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="insert"
          migrationMode={migrationMode}
          currentSchema={currentSchema}
        />
        <br />
        <div className={styles.insertContainer + ' container-fluid'}>
          <div className="col-xs-9">
            <form className="form-horizontal">
              {elements}
              <Button
                type="submit"
                color="yellow"
                size="sm"
                onClick={e => {
                  e.preventDefault();
                  dispatch({ type: E_ONGOING_REQ });
                  const inputValues = {};
                  Object.keys(refs).map(colName => {
                    if (refs[colName].nullNode.checked) {
                      // null
                      inputValues[colName] = null;
                    } else if (refs[colName].defaultNode.checked) {
                      // default
                      return;
                    } else {
                      inputValues[colName] = refs[colName].valueNode.value; // TypedInput is an input inside a div
                    }
                  });
                  dispatch(editItem(tableName, inputValues));
                }}
                data-test="save-button"
              >
                {buttonText}
              </Button>
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
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    ...state.tables.update,
    schemas: state.tables.allSchemas,
    migrationMode: state.main.migrationMode,
    currentSchema: state.tables.currentSchema,
  };
};

const editItemConnector = connect => connect(mapStateToProps)(EditItem);

export default editItemConnector;
