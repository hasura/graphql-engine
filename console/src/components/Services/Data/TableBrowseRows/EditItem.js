import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { editItem, E_ONGOING_REQ } from './EditActions';
import globals from '../../../../Globals';
import { modalClose } from './EditActions';

// import RichTextEditor from 'react-rte';
import { replace } from 'react-router-redux';

class EditItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, editorColumnMap: {}, currentColumn: null };
  }

  onTextChange = (e, colName) => {
    const textValue = e.target.value;
    const tempState = {
      ...this.state,
    };
    tempState.editorColumnMap = { ...this.state.editorColumnMap };
    tempState.editorColumnMap[colName] = textValue;
    this.setState({ ...tempState });
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
          placeholder="text"
          type="text"
          className={'form-control ' + styles.insertBox}
          onClick={clicker}
          ref={inputRef}
          defaultValue={oldItem[colName]}
          data-test={`typed-input-${i}`}
        />
      );

      // Integer
      if (colType === 'integer') {
        typedInput = (
          <input
            placeholder="integer"
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'numeric') {
        typedInput = (
          <input
            placeholder="float"
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'timestamp with time zone') {
        typedInput = (
          <input
            placeholder={new Date().toISOString()}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'date') {
        typedInput = (
          <input
            placeholder={new Date().toISOString().slice(0, 10)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'timetz') {
        const time = new Date().toISOString().slice(11, 19);
        typedInput = (
          <input
            placeholder={`${time}Z or ${time}+05:30`}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={oldItem[colName]}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'json' || colType === 'jsonb') {
        typedInput = (
          <input
            placeholder={'{"name": "foo"} or [12, "asdf"]'}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={JSON.stringify(oldItem[colName])}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'boolean') {
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
              <button
                type="submit"
                className={styles.yellow_button}
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
              </button>
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
