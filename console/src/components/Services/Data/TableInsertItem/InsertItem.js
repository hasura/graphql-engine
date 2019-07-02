import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import { insertItem, I_RESET } from './InsertActions';
import { ordinalColSort } from '../utils';
import { setTable } from '../DataActions';
import JsonInput from '../../../Common/CustomInputTypes/JsonInput';
import Button from '../../../Common/Button/Button';
import { getPlaceholder, BOOLEAN, JSONB, JSONDTYPE } from '../utils';

import { getParentNodeByClass } from '../../../../utils/domFunctions';

import { NotFoundError } from '../../../Error/PageNotFound';

class InsertItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0 };
  }

  componentDidMount() {
    this.props.dispatch(setTable(this.props.tableName));
  }

  componentWillUnmount() {
    this.props.dispatch({ type: I_RESET });
  }

  nextInsert() {
    // when use state object remember to do it inside a class method.
    // Since the state variable lifecycle is tired to the instance of the class
    // and making this change using an anonymous function will case errors.
    this.setState({
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

    const styles = require('../../../Common/TableCommon/Table.scss');
    // check if table exists
    const currentTable = schemas.find(
      s => s.table_name === tableName && s.table_schema === currentSchema
    );
    if (!currentTable) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const _columns = schemas.find(
      x => x.table_name === tableName && x.table_schema === currentSchema
    ).columns;
    const refs = {};
    const columns = _columns.sort(ordinalColSort);

    const elements = columns.map((col, i) => {
      const colName = col.column_name;
      const hasDefault = col.column_default && col.column_default.trim() !== '';
      const isNullable = col.is_nullable && col.is_nullable !== 'NO';

      refs[colName] = { valueNode: null, nullNode: null, defaultNode: null };
      const inputRef = node => (refs[colName].valueNode = node);
      const clicker = e => {
        const checkboxLabel = getParentNodeByClass(e.target, 'radio-inline');
        if (checkboxLabel) {
          checkboxLabel.click();
        } else {
          e.target.parentNode.click();
        }
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

      const standardInputProps = {
        className: `form-control ${styles.insertBox}`,
        'data-test': `typed-input-${i}`,
        defaultValue: clone && colName in clone ? clone[colName] : '',
        onClick: clicker,
        onChange: (e, val) => {
          if (isAutoIncrement) return;
          if (!isNullable && !hasDefault) return;

          const textValue = typeof val === 'string' ? val : e.target.value;

          const radioToSelectWhenEmpty = hasDefault
            ? refs[colName].defaultNode
            : refs[colName].nullNode;
          refs[colName].insertRadioNode.checked = !!textValue.length;
          radioToSelectWhenEmpty.checked = !textValue.length;
        },
        onFocus: e => {
          if (isAutoIncrement) return;
          if (!isNullable && !hasDefault) return;
          const textValue = e.target.value;
          if (
            textValue === undefined ||
            textValue === null ||
            textValue.length === 0
          ) {
            const radioToSelectWhenEmpty = hasDefault
              ? refs[colName].defaultNode
              : refs[colName].nullNode;

            refs[colName].insertRadioNode.checked = false;
            radioToSelectWhenEmpty.checked = true;
          }
        },
        placeholder: 'text',
        ref: inputRef,
        type: 'text',
      };

      const colType = col.data_type;
      const placeHolder = hasDefault
        ? col.column_default
        : getPlaceholder(colType);

      let typedInput = (
        <input {...standardInputProps} placeholder={placeHolder} />
      );

      if (isAutoIncrement) {
        typedInput = (
          <input {...standardInputProps} readOnly placeholder={placeHolder} />
        );
      }

      if (colType === JSONDTYPE || colType === JSONB) {
        // JSON/JSONB
        typedInput = (
          <JsonInput
            standardProps={standardInputProps}
            placeholderProp={getPlaceholder(colType)}
          />
        );
      }

      if (colType === BOOLEAN) {
        // Boolean
        typedInput = (
          <select
            {...standardInputProps}
            onClick={e => {
              e.target.parentNode.parentNode.click();
              e.target.focus();
            }}
            defaultValue={placeHolder}
          >
            <option value="" disabled>
              -- bool --
            </option>
            <option value="true">True</option>
            <option value="false">False</option>
          </select>
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
              defaultChecked={!hasDefault & !isNullable}
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
            <input
              disabled={!hasDefault}
              type="radio"
              ref={node => {
                refs[colName].defaultNode = node;
              }}
              name={colName + '-value'}
              value="option3"
              defaultChecked={hasDefault}
              data-test={`typed-input-default-${i}`}
            />
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
              <Button
                type="submit"
                color="yellow"
                size="sm"
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
                      inputValues[colName] =
                        refs[colName].valueNode.props !== undefined
                          ? refs[colName].valueNode.props.value
                          : refs[colName].valueNode.value;
                    }
                  });
                  dispatch(insertItem(tableName, inputValues)).then(() => {
                    this.nextInsert();
                  });
                }}
                data-test="insert-save-button"
              >
                {this.state.insertedRows > 0 ? 'Insert Again' : 'Save'}
              </Button>
              <Button
                color="white"
                size="sm"
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
