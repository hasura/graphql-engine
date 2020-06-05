import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import { ordinalColSort } from '../utils';

import { insertItem, I_RESET, fetchEnumOptions } from './InsertActions';
import { setTable } from '../DataActions';
import { NotFoundError } from '../../../Error/PageNotFound';
import {
  findTable,
  generateTableDef,
  isColumnAutoIncrement,
} from '../../../Common/utils/pgUtils';
import { TypedInput } from '../Common/Components/TypedInput';
import styles from '../../../Common/TableCommon/Table.scss';

class InsertItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, isMigrationChecked: false };
  }

  componentDidMount() {
    this.props.dispatch(setTable(this.props.tableName));
    this.props.dispatch(fetchEnumOptions());
  }

  componentWillUnmount() {
    this.props.dispatch({ type: I_RESET });
  }

  nextInsert() {
    // when use state object remember to do it inside a class method.
    // Since the state variable lifecycle is tied to the instance of the class
    // and making this change using an anonymous function will cause errors.
    this.setState(prev => ({
      insertedRows: prev.insertedRows + 1,
    }));
  }

  toggleMigrationCheckBox = () => {
    this.setState(
      prevState => ({
        isMigrationChecked: !prevState.isMigrationChecked,
      }),
      () => {
        // TODO: insert code to execute here?
        // if the checkbox is checked then fundamental changes
        // have to be made to how the Save functionality would workx
      }
    );
  };

  render() {
    const {
      tableName,
      currentSchema,
      clone,
      schemas,
      migrationMode,
      readOnlyMode,
      ongoingRequest,
      lastError,
      lastSuccess,
      count,
      dispatch,
      enumOptions,
    } = this.props;

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, currentSchema)
    );

    // check if table exists
    if (!currentTable) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const columns = currentTable.columns.sort(ordinalColSort);

    const refs = {};

    const elements = columns.map((col, i) => {
      const colName = col.column_name;
      const hasDefault = col.column_default && col.column_default.trim() !== '';
      const isNullable = col.is_nullable && col.is_nullable !== 'NO';
      const isIdentity = col.is_identity && col.is_identity !== 'NO';
      const isAutoIncrement = isColumnAutoIncrement(col);

      refs[colName] = { valueNode: null, nullNode: null, defaultNode: null };

      const onChange = (e, val) => {
        if (isAutoIncrement) return;
        if (!isNullable && !hasDefault) return;

        const textValue = typeof val === 'string' ? val : e.target.value;

        const radioToSelectWhenEmpty =
          hasDefault || isIdentity
            ? refs[colName].defaultNode
            : refs[colName].nullNode;

        refs[colName].insertRadioNode.checked = !!textValue.length;
        radioToSelectWhenEmpty.checked = !textValue.length;
      };
      const onFocus = e => {
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
      };

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
            <TypedInput
              inputRef={node => {
                refs[colName].valueNode = node;
              }}
              enumOptions={enumOptions}
              col={col}
              clone={clone}
              onChange={onChange}
              onFocus={onFocus}
              index={i}
            />
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
              type="radio"
              ref={node => {
                refs[colName].defaultNode = node;
              }}
              name={colName + '-value'}
              value="option3"
              disabled={!hasDefault && !isIdentity}
              defaultChecked={hasDefault || isIdentity}
              data-test={`typed-input-default-${i}`}
            />
            <span className={styles.radioSpan}>Default</span>
          </label>
        </div>
      );
    });

    let alert = null;
    let buttonText = this.state.insertedRows > 0 ? 'Insert Again' : 'Save';
    if (ongoingRequest) {
      alert = (
        <div className="hidden alert alert-warning" role="alert">
          Inserting...
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
          Inserted! <br /> {JSON.stringify(lastSuccess)}
        </div>
      );
    }

    return (
      <div className={styles.container + ' container-fluid'}>
        <TableHeader
          count={count}
          dispatch={dispatch}
          table={currentTable}
          tabName="insert"
          migrationMode={migrationMode}
          readOnlyMode={readOnlyMode}
        />
        <br />
        <div className={styles.insertContainer + ' container-fluid'}>
          <div className="col-xs-9">
            <form id="insertForm" className="form-horizontal">
              {elements}
              <div className={styles.display_flex}>
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
                  {buttonText}
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
                <ReloadEnumValuesButton
                  dispatch={dispatch}
                  isEnum={currentTable.is_enum}
                />
                <label className={styles.labelText}>
                  <input
                    type="checkbox"
                    checked={this.state.isMigrationChecked}
                    title="This is a Migration"
                    onChange={this.toggleMigrationCheckBox}
                    className={styles.migrationCheckbox}
                  />
                  This is a Migration
                </label>
              </div>
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
    ...state.tables.insert,
    schemas: state.tables.allSchemas,
    ...state.tables.view,
    migrationMode: state.main.migrationMode,
    readOnlyMode: state.main.readOnlyMode,
    currentSchema: state.tables.currentSchema,
  };
};

const insertItemConnector = connect => connect(mapStateToProps)(InsertItem);

export default insertItemConnector;
