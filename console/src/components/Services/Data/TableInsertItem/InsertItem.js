import PropTypes from 'prop-types';
import React, { Component } from 'react';
import TableHeader from '../TableCommon/TableHeader';
import Button from '../../../Common/Button/Button';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import { ordinalColSort } from '../utils';

import { insertItem, I_RESET, fetchEnumOptions } from './InsertActions';
import { setTable } from '../DataActions';
import { NotFoundError } from '../../../Error/PageNotFound';
import { findTable, isFeatureSupported } from '../../../../dataSources';
import styles from '../../../Common/TableCommon/Table.scss';
import { TableRow } from '../Common/Components/TableRow';
import { generateTableDef } from '../../../../dataSources';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import MigrationCheckbox from './MigrationCheckbox';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import FeatureDisabled from '../FeatureDisabled';

class InsertItem extends Component {
  constructor() {
    super();
    this.state = { insertedRows: 0, isMigration: false };
  }

  componentDidMount() {
    const { tableName, dispatch } = this.props;
    dispatch(setTable(tableName));
    dispatch(fetchEnumOptions());
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
    this.setState(prev => ({ isMigration: !prev.isMigration }));
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
      currentSource,
    } = this.props;

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, currentSchema)
    );

    if (!isFeatureSupported('tables.insert.enabled')) {
      return (
        <FeatureDisabled
          tab="insert"
          tableName={tableName}
          schemaName={currentSchema}
        />
      );
    }

    // check if table exists
    if (!currentTable && isFeatureSupported('tables.insert.enabled')) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const isCLIMode = globals.consoleMode === CLI_CONSOLE_MODE;

    const columns = currentTable.columns.sort(ordinalColSort);

    const refs = {};

    const elements = columns.map((col, i) => {
      const {
        column_name: colName,
        is_identity: isIdentity,
        column_default,
      } = col;
      const hasDefault = column_default && column_default.trim() !== '';

      refs[colName] = {
        valueNode: null,
        nullNode: null,
        defaultNode: null,
        insertRadioNode: null,
      };

      const onChange = (e, val) => {
        const textValue = typeof val === 'string' ? val : e.target.value;

        const radioToSelectWhenEmpty =
          hasDefault || isIdentity
            ? refs[colName].defaultNode
            : refs[colName].nullNode;

        refs[colName].insertRadioNode.checked = !!textValue.length;
        radioToSelectWhenEmpty.checked = !textValue.length;
      };
      const onFocus = e => {
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
        <TableRow
          key={i}
          column={col}
          setRef={(key, node) => (refs[colName][key] = node)}
          enumOptions={enumOptions}
          index={i}
          clone={clone}
          onChange={onChange}
          onFocus={onFocus}
        />
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

    const onClickClear = () => {
      const form = document.getElementById('insertForm');
      const inputs = form.getElementsByTagName('input');
      Array.from(inputs).forEach(input => {
        switch (input.type) {
          case 'text':
            input.value = '';
            break;
          case 'radio':
          case 'checkbox':
            break;
          default:
        }
      });
    };

    const onClickSave = e => {
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
      dispatch(insertItem(tableName, inputValues, this.state.isMigration)).then(
        () => {
          this.nextInsert();
        }
      );
    };

    return (
      <RightContainer>
        <div className={styles.container + ' container-fluid'}>
          <TableHeader
            count={count}
            dispatch={dispatch}
            table={currentTable}
            source={currentSource}
            tabName="insert"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
          />
          <br />
          <div className={styles.insertContainer + ' container-fluid'}>
            <div className="col-xs-9">
              <form id="insertForm" className="form-horizontal">
                <div className={styles.form_flex}>
                  {elements}
                  <MigrationCheckbox
                    onChange={this.toggleMigrationCheckBox}
                    isChecked={this.state.isMigration}
                    isCLIMode={isCLIMode}
                  />
                </div>
                <div className={styles.display_flex}>
                  <Button
                    type="submit"
                    color="yellow"
                    size="sm"
                    onClick={onClickSave}
                    data-test="insert-save-button"
                  >
                    {buttonText}
                  </Button>
                  <Button
                    color="white"
                    size="sm"
                    onClick={onClickClear}
                    data-test="clear-button"
                  >
                    Clear
                  </Button>
                  {currentTable.is_enum ? (
                    <ReloadEnumValuesButton dispatch={dispatch} />
                  ) : null}
                </div>
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
    currentSource: state.tables.currentDataSource,
  };
};

const insertItemConnector = connect => connect(mapStateToProps)(InsertItem);

export default insertItemConnector;
