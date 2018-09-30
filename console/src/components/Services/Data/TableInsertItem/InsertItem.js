import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { insertItem, I_RESET } from './InsertActions';
import { ordinalColSort } from '../utils';
import { modalOpen, modalClose } from './InsertActions';
import { setTable } from '../DataActions';

// import RichTextEditor from 'react-rte';

class InsertItem extends Component {
  constructor() {
    super();
    // this.state = {insertedRows: 0, editorState: RichTextEditor.createEmptyValue(), editorColumnMap: {}, currentColumn: null};
    this.state = { insertedRows: 0, editorColumnMap: {}, currentColumn: null };
  }

  componentDidMount() {
    this.props.dispatch(setTable(this.props.tableName));
  }

  componentWillUnmount() {
    this.props.dispatch({ type: I_RESET });
  }

  onSwitchClick = (e, colName, clone) => {
    this.props.dispatch(modalOpen());
    // get value of current column
    let currentValue = this.state.editorColumnMap[colName];
    // if value exists, set editorState to that value, else empty it
    if (currentValue !== undefined) {
      // this.setState({editorState: RichTextEditor.createValueFromString(currentValue, 'html')});
    } else if (clone !== undefined && clone !== null && colName in clone) {
      currentValue = clone[colName];
      // this.setState({editorState: RichTextEditor.createValueFromString(clone[colName], 'html')});
    } else {
      // this.setState({editorState: RichTextEditor.createEmptyValue()});
    }
    this.setState({ currentColumn: colName });
  };

  onTextChange = (e, colName, { insertRadioNode, nullNode }) => {
    const textValue = e.target.value;
    const tempState = {
      ...this.state,
    };
    tempState.editorColumnMap = { ...this.state.editorColumnMap };
    tempState.editorColumnMap[colName] = textValue;
    if (!nullNode.disabled) {
      insertRadioNode.checked = !!textValue.length;
      nullNode.checked = !textValue.length;
    }
    this.setState({ ...tempState });
  };

  onTextFocus = (e, { nullNode }) => {
    if (nullNode.disabled) return;
    const textValue = e.target.value;
    if (
      textValue === undefined ||
      textValue === null ||
      textValue.length === 0
    ) {
      nullNode.checked = true;
    }
  };

  onModalClose = () => {
    this.props.dispatch(modalClose());
  };

  textEditorChange = editorState => {
    // maintain column level state
    const htmlValue = editorState.toString('html');
    // const currentColumn = this.state.currentColumn;
    const mapObject = this.state.editorColumnMap;
    const currentColumn = this.state.currentColumn;
    mapObject[currentColumn] = htmlValue;
    this.setState({ editorState });
    this.setState({ ...this.state.editorColumnMap, mapObject });
  };

  rteClicked = () => {
    this.refs.editor.focus();
  };

  nextInsert() {
    // when use state object remember to do it inside a class method.
    // Since the state variable lifecycle is tired to the instance of the class
    // and making this change using an anonymous function will case errors.
    this.setState({
      ...this.state,
      insertedRows: this.state.insertedRows + 1,
    });
  }

  render() {
    const {
      tableName,
      clone,
      schemas,
      currentSchema,
      migrationMode,
      ongoingRequest,
      lastError,
      lastSuccess,
      count,
      dispatch,
    } = this.props;

    const styles = require('../TableCommon/Table.scss');
    const _columns = schemas.find(x => x.table_name === tableName).columns;
    const refs = {};
    const columns = _columns.sort(ordinalColSort);

    const elements = columns.map((col, i) => {
      const colName = col.column_name;
      const isDefault = col.column_default && col.column_default.trim() !== '';
      const isNullable = col.is_nullable && col.is_nullable !== 'NO';

      refs[colName] = { valueNode: null, nullNode: null, defaultNode: null };
      const inputRef = node => (refs[colName].valueNode = node);
      const clicker = e => {
        e.target.parentNode.click();
        e.target.focus();
      };
      const colDefault = col.column_default;
      let isAutoIncrement = false;
      if (
        colDefault ===
        "nextval('" + tableName + '_' + colName + "_seq'::regclass)"
      ) {
        isAutoIncrement = true;
      }
      // Text type
      // auto-increment format    nextval('tablename_columnname_seq'::regclass)
      let typedInput = (
        <input
          placeholder="text"
          type="text"
          className={'form-control ' + styles.insertBox}
          onClick={clicker}
          ref={inputRef}
          defaultValue={clone && colName in clone ? clone[colName] : ''}
          data-test={`typed-input-${i}`}
        />
      );

      const colType = col.data_type;
      // Integer
      if (colType === 'integer') {
        // check if autoincrement
        if (isAutoIncrement) {
          typedInput = (
            <input
              readOnly
              placeholder="integer"
              type="text"
              className={'form-control ' + styles.insertBox}
              onClick={clicker}
              ref={inputRef}
              defaultValue={clone && colName in clone ? clone[colName] : ''}
              data-test={`typed-input-${i}`}
            />
          );
        } else {
          typedInput = (
            <input
              placeholder="integer"
              type="text"
              className={'form-control ' + styles.insertBox}
              onClick={clicker}
              ref={inputRef}
              defaultValue={clone && colName in clone ? clone[colName] : ''}
              data-test={`typed-input-${i}`}
            />
          );
        }
      } else if (colType === 'bigint') {
        typedInput = (
          <input
            placeholder="BIG integer"
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'numeric') {
        // Numeric
        typedInput = (
          <input
            placeholder="float"
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'timestamp with time zone') {
        // Timestamp
        typedInput = (
          <input
            placeholder={new Date().toISOString()}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'date') {
        // Date
        typedInput = (
          <input
            placeholder={new Date().toISOString().slice(0, 10)}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'timetz') {
        // Time
        const time = new Date().toISOString().slice(11, 19);
        typedInput = (
          <input
            placeholder={`${time}Z or ${time}+05:30`}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'uuid') {
        // UUID
        typedInput = (
          <input
            placeholder={'UUID'}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'json' || colType === 'jsonb') {
        // JSON/JSONB
        typedInput = (
          <input
            placeholder={'{"name": "foo"} or [12, "asdf"]'}
            type="text"
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={
              clone && colName in clone ? JSON.stringify(clone[colName]) : ''
            }
            data-test={`typed-input-${i}`}
          />
        );
      } else if (colType === 'boolean') {
        // Boolean
        typedInput = (
          <select
            className={'form-control ' + styles.insertBox}
            onClick={clicker}
            ref={inputRef}
            defaultValue={clone && colName in clone ? clone[colName] : ''}
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
        if (this.state.editorColumnMap[colName] !== undefined) {
          defaultValue = this.state.editorColumnMap[colName];
          currentValue = this.state.editorColumnMap[colName];
        } else if (clone && colName in clone) {
          defaultValue = clone[colName];
        }
        if (currentValue !== '') {
          /*
          typedInput = (<span><input placeholder={'text'} type="text" className={'form-control ' + styles.insertBox} onClick={clicker} ref={inputRef} onChange={e => { this.onTextChange(e, colName); }} value={currentValue} />
            <span onClick={e => { this.onSwitchClick(e, colName, clone); }}>Switch to Rich Text Editor</span>
            </span>
          );
          */
          typedInput = (
            <span>
              <input
                placeholder={'text'}
                type="text"
                className={'form-control ' + styles.insertBox}
                onClick={clicker}
                ref={inputRef}
                onChange={e => {
                  this.onTextChange(e, colName, refs[colName]);
                }}
                onFocus={e => {
                  this.onTextFocus(e, refs[colName]);
                }}
                value={currentValue}
                data-test={`typed-input-${i}`}
              />
            </span>
          );
        } else {
          /*
          typedInput = (<span><input placeholder={'text'} type="text" className={'form-control ' + styles.insertBox} onClick={clicker} ref={inputRef} onChange={e => { this.onTextChange(e, colName); }} value={defaultValue} />
            <span onClick={e => { this.onSwitchClick(e, colName, clone); }}>Switch to Rich Text Editor</span>
            </span>
          );
          */
          typedInput = (
            <span>
              <input
                placeholder={'text'}
                type="text"
                className={'form-control ' + styles.insertBox}
                onClick={clicker}
                ref={inputRef}
                onChange={e => {
                  this.onTextChange(e, colName, refs[colName]);
                }}
                onFocus={e => {
                  this.onTextFocus(e, refs[colName]);
                }}
                value={defaultValue}
                data-test={`typed-input-${i}`}
              />
            </span>
          );
        }

        // typedInput = (<span><input placeholder={'text'} type="text" className={'form-control ' + styles.insertBox} onClick={clicker} ref={inputRef} defaultValue={(clone && colName in clone) ? JSON.stringify(clone[colName]) : ''} />);

        // typedInput = (<RichTextEditor placeholder="Enter some text" value={this.state.editorState} ref="editor" onClick={this.rteClicked} onChange={this.textEditorChange} toolbarConfig={toolbarConfig} />);
      }

      let showDefaultOption = (
        <input
          type="radio"
          ref={node => {
            refs[colName].defaultNode = node;
          }}
          name={colName + '-value'}
          value="option3"
          defaultChecked={isDefault}
          data-test={`typed-input-default-${i}`}
        />
      );
      if (!isDefault) {
        showDefaultOption = (
          <input
            disabled
            type="radio"
            ref={node => {
              refs[colName].defaultNode = node;
            }}
            name={colName + '-value'}
            value="option3"
            defaultChecked={isDefault}
            data-test={`typed-input-default-${i}`}
          />
        );
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
              disabled={isAutoIncrement}
              type="radio"
              ref={node => {
                refs[colName].insertRadioNode = node;
              }}
              name={colName + '-value'}
              value="option1"
              defaultChecked={!isDefault & !isNullable}
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
              defaultChecked={isNullable}
              name={colName + '-value'}
              value="NULL"
              data-test={`nullable-radio-${i}`}
            />
            <span className={styles.radioSpan}>NULL</span>
          </label>
          <label className={styles.radioLabel + ' radio-inline'}>
            {showDefaultOption}
            <span className={styles.radioSpan}>Default</span>
          </label>
        </div>
      );
    });

    let alert = null;
    if (ongoingRequest) {
      alert = (
        <div className="hidden alert alert-warning" role="alert">
          Inserting...
        </div>
      );
    } else if (lastError) {
      alert = (
        <div className="hidden alert alert-danger" role="alert">
          Error: {JSON.stringify(lastError)}
        </div>
      );
    } else if (lastSuccess) {
      alert = (
        <div className="hidden alert alert-success" role="alert">
          Inserted! <br /> {JSON.stringify(lastSuccess)}
        </div>
      );
    }

    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          tableName={tableName}
          tabName="insert"
          migrationMode={migrationMode}
          currentSchema={currentSchema}
        />
        <br />
        <div className={styles.insertContainer + ' container-fluid'}>
          <div className="col-xs-9">
            <form id="insertForm" className="form-horizontal">
              {elements}
              <button
                type="submit"
                className={'btn ' + styles.yellow_button}
                onClick={e => {
                  e.preventDefault();
                  const inputValues = {};
                  Object.keys(refs).map(colName => {
                    if (refs[colName].nullNode.checked) {
                      // null
                      inputValues[colName] = null;
                    } else if (refs[colName].defaultNode.checked) {
                      // default
                      return;
                    } else {
                      inputValues[colName] = refs[colName].valueNode.value;
                    }
                  });
                  dispatch(insertItem(tableName, inputValues)).then(() => {
                    this.nextInsert();
                  });
                }}
                data-test="insert-save-button"
              >
                {this.state.insertedRows > 0 ? 'Insert Again' : 'Save'}
              </button>
              <button
                className={'btn ' + styles.default_button}
                onClick={e => {
                  e.preventDefault();
                  const form = document.getElementById('insertForm');
                  const inputs = form.getElementsByTagName('input');
                  for (let i = 0; i < inputs.length; i++) {
                    switch (inputs[i].type) {
                      // case 'hidden':
                      case 'text':
                        inputs[i].value = '';
                        break;
                      case 'radio':
                      case 'checkbox':
                        // inputs[i].checked = false;
                        break;
                      default:
                      // pass
                    }
                  }
                }}
                data-test="clear-button"
              >
                Clear
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

InsertItem.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  clone: PropTypes.object,
  schemas: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastSuccess: PropTypes.object,
  lastError: PropTypes.object,
  isModalOpen: PropTypes.bool,
  count: PropTypes.number,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => {
  return {
    tableName: ownProps.params.table,
    ...state.tables.insert,
    schemas: state.tables.allSchemas,
    ...state.tables.view,
    migrationMode: state.main.migrationMode,
    currentSchema: state.tables.currentSchema,
  };
};

const insertItemConnector = connect => connect(mapStateToProps)(InsertItem);

export default insertItemConnector;
