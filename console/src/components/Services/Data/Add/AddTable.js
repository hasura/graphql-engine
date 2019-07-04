import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';

import Button from '../../../Common/Button/Button';
import PrimaryKeySelector from '../Common/ReusableComponents/PrimaryKeySelector';
import ForeignKeyWrapper from './ForeignKeyWrapper';
import UniqueKeyWrapper from './UniqueKeyWrapper';

import { showErrorNotification } from '../../Common/Notification';

import TableName from './TableName';
import TableColumns from './TableColumns';
import TableComment from './TableComment';

import * as tooltip from './Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import {
  setTableName,
  setTableComment,
  removeColumn,
  setColName,
  setColType,
  setColNullable,
  setColDefault,
  setForeignKeys,
  setUniqueKeys,
} from './AddActions';

import { fetchColumnTypeInfo, RESET_COLUMN_TYPE_INFO } from '../DataActions';
import { setDefaults, setPk, createTableSql } from './AddActions';
import { resetValidation } from './AddActions';

import gqlPattern, {
  gqlTableErrorNotif,
  gqlColumnErrorNotif,
  gqlTableNameNullNotif,
  gqlTableEnufColumns,
  gqlColumnNoDups,
  gqlColumnTypes,
  gqlColumnDefaults,
  gqlMinPrimaryKey,
} from '../Common/GraphQLValidation'; // TODO add the others

/* AddTable is a wrapper which wraps
 *  1) Table Name input
 *  2) Columns inputs
 *  3) Primary Key input
 *  4) Comment Input
 *  5) Add Table button
 * */

class AddTable extends Component {
  constructor(props) {
    super(props);

    this.props.dispatch(setDefaults());

    this.onTableNameChange = this.onTableNameChange.bind(this);
    this.onTableCommentChange = this.onTableCommentChange.bind(this);
    this.onRemoveColumn = this.onRemoveColumn.bind(this);
    this.onColumnChange = this.onColumnNameChange.bind(this);
    this.onColTypeChange = this.onColTypeChange.bind(this);
    this.onColNullableChange = this.onColNullableChange.bind(this);
    this.onColUniqueChange = this.onColUniqueChange.bind(this);
    this.setColDefaultValue = this.setColDefaultValue.bind(this);

    this.trimEmptyColumns = this.trimEmptyColumns.bind(this);
    this.checkAndDispatch = this.checkAndDispatch.bind(this);
    this.validateAndSubmit = this.validateAndSubmit.bind(this);

    this.tableNameCheck = this.tableNameCheck.bind(this);
    this.validateEnoughColumns = this.validateEnoughColumns.bind(this);
    this.validateColumnNames = this.validateColumnNames.bind(this);
    this.validateNoDupNames = this.validateNoDupNames.bind(this);
    this.validateColumnTypes = this.validateColumnTypes.bind(this);
    this.validateColumnDefaults = this.validateColumnDefaults.bind(this);
    this.minPrimaryKeyCheck = this.minPrimaryKeyCheck.bind(this);
    this.isModified = this.isModified.bind(this);
    this.isValidType = this.isValidType.bind(this);
    this.isValidDefault = this.isValidDefault.bind(this);
  }

  componentDidMount() {
    this.props.dispatch(fetchColumnTypeInfo());
  }

  componentWillUnmount() {
    this.props.dispatch(setDefaults());
    this.props.dispatch({
      type: RESET_COLUMN_TYPE_INFO,
    });
  }

  onTableNameChange = e => {
    const { dispatch } = this.props;
    dispatch(setTableName(e.target.value));
  };

  onTableCommentChange = e => {
    const { dispatch } = this.props;
    dispatch(setTableComment(e.target.value));
  };

  onRemoveColumn = i => {
    const { dispatch } = this.props;
    dispatch(removeColumn(i));
  };

  onColumnNameChange = (i, isNullableChecked, e) => {
    const { dispatch } = this.props;
    dispatch(setColName(e.target.value, i, isNullableChecked));
  };

  onColTypeChange = (i, value) => {
    const { dispatch } = this.props;
    dispatch(setColType(value, i));
  };
  onColNullableChange = (i, e) => {
    const { dispatch } = this.props;
    dispatch(setColNullable(e.target.checked, i));
  };

  onColUniqueChange = (i, numUniqueKeys, isColumnUnique, _uindex) => {
    const { dispatch, uniqueKeys } = this.props;
    if (isColumnUnique) {
      dispatch(
        setUniqueKeys([
          ...uniqueKeys.slice(0, _uindex),
          ...uniqueKeys.slice(_uindex + 1),
        ])
      );
    } else {
      const newUniqueKeys = JSON.parse(JSON.stringify(uniqueKeys));
      newUniqueKeys[numUniqueKeys - 1] = [i];
      dispatch(setUniqueKeys([...newUniqueKeys, []]));
    }
  };

  setColDefaultValue = (i, isNullableChecked, value) => {
    const { dispatch } = this.props;
    dispatch(setColDefault(value, i, isNullableChecked));
  };

  minPrimaryKeyCheck() {
    return this.props.primaryKeys.filter(key => key !== '').length > 0;
  }

  // check the validity and if invalid, dispatch
  // valid values are true and ""
  // strings get interpolated with notificationArray
  // and objects are assumed to be an array like notificationArray
  // and the second arg is ignored
  checkAndDispatch(validated, notificationArray) {
    if (validated === true || validated === '') return true;
    else if (validated === false) {
      this.props.dispatch(
        showErrorNotification(
          notificationArray[0],
          notificationArray[1],
          notificationArray[2],
          notificationArray[3]
        )
      );
      return false;
    } else if (typeof validated === 'string') {
      this.props.dispatch(
        showErrorNotification(
          notificationArray[0],
          validated,
          notificationArray[2],
          { custom: validated }
        )
      );
      return false;
    } else if (typeof validated === 'object') {
      this.props.dispatch(
        showErrorNotification(
          notificationArray[0],
          notificationArray[1],
          notificationArray[2],
          notificationArray[3]
        )
      );
      return false;
    }
  }

  tableNameCheck() {
    return gqlPattern.test(this.props.tableName);
  }

  isModified(x) {
    if (x === undefined) return false;
    else if (typeof x === 'string' && /^..*$/.test(x)) return true;

    return false;
  }

  // return a shallow copy of a columns array, trimming those columns with
  // no non-default values from the end. Invalids in the middle retained
  trimEmptyColumns(Columns) {
    let c = Columns.slice(0);

    for (let i = c.length - 1; i >= 0; i--) {
      if (
        this.isModified(c[i].name) ||
        this.isModified(c[i].type) ||
        this.isModified(c[i].default)
      ) {
        return c;
      }
      c = c.slice(0, c.length - 1);
    }
    return c;
  }

  validateEnoughColumns(cols) {
    return cols.length > 0;
  }

  validateColumnNames(cols) {
    const l = cols.length;

    for (let i = 0; i < l; i++) {
      if (!gqlPattern.test(cols[i].name)) {
        return (
          'Invalid name for column ' +
          i.toString() +
          ', ' +
          cols[i].name.toString() +
          ' is Invalid. ' +
          'Column names must be letters, numbers, and _ and start with a letter'
        );
      }
    }
    return '';
  }

  isValidType(s) {
    return (typeof s === 'string' &&
            s.trim().length > 0);
  }

  validateColumnTypes(cols) {
    const l = cols.length;
    for (let i = 0; i < l; i++) {
      if (!this.isValidType(cols[i].type)) {
        return (
          'Invalid type (' +
          cols[i].type.toString() +
          ') for column ' +
          i.toString() +
          ' (' +
          cols[i].name.toString() +
          ')'
        );
      }
    }
    return '';
  }

  validateNoDupNames(cols) {
    const l = cols.length;
    const names = [];

    for (let j = 0; j < l; j++) {
      names.push(cols[j].name);
    }

    for (let i = 0; i < l; i++) {
      const li = names.lastIndexOf(cols[i].name);
      if (li !== -1 && li !== i) {
        // slightly tricky code, we just have to find a dup, not all
        return (
          'Column ' +
          i.toString() +
          ', ' +
          cols[i].name.toString() +
          ' is duplicated by column ' +
          li.toString()
        );
      }
    }
    return '';
  }

  /* eslint-disable no-unused-vars */
  isValidDefault(type, d) {
    return true;
  }
  /* eslint-enable no-unused-vars */

  /* punting for now
    isValidDefault(type, d) {
        if(d === null || d === "")
            return true;

        switch (type) {
            case "integer":
            case "serial":
            case "bigint":
            case "bigserial":
                return (parseInt(d) !== NaN);
            case "boolean":
                return (d === "true" || d === "false");
            case "numeric":
            case "float":
                return (parseFloat(d) !== NaN);
            case "timestamptz":
            case "timestamp":
            case "timetz":
            case "date":
                // TODO
            case "UUID":
            // TODO
            case "JSONB":
            // TODO
            case "text":
            default:
                    return true;
        }
    }
*/

  validateColumnDefaults(cols) {
    const l = cols.length;
    for (let i = 0; i < l; i++) {
      if (!this.isValidDefault(cols[i].type, cols[i].default)) {
        return (
          'Invalid default (' +
          cols[i].default.toString() +
          ') for column ' +
          i.toString() +
          ' (' +
          cols[i].name.toString() +
          ')'
        );
      }
    }
    return '';
  }

  // UI has decided we're ready to submit. Validate that the
  // props have a valid new table, and ask server for new table
  validateAndSubmit() {
    this.props.dispatch(resetValidation());
    const validColumns = this.trimEmptyColumns(this.props.columns);

    if (
      this.checkAndDispatch(
        this.props.tableName !== null,
        gqlTableNameNullNotif
      ) &&
      this.checkAndDispatch(this.tableNameCheck(), gqlTableErrorNotif) &&
      this.checkAndDispatch(
        this.validateEnoughColumns(validColumns),
        gqlTableEnufColumns
      ) &&
      this.checkAndDispatch(
        this.validateColumnNames(validColumns),
        gqlColumnErrorNotif
      ) &&
      this.checkAndDispatch(
        this.validateNoDupNames(validColumns),
        gqlColumnNoDups
      ) &&
      this.checkAndDispatch(
        this.validateColumnTypes(validColumns),
        gqlColumnTypes
      ) &&
      this.checkAndDispatch(
        this.validateColumnDefaults(validColumns),
        gqlColumnDefaults
      ) &&
      this.checkAndDispatch(this.minPrimaryKeyCheck(), gqlMinPrimaryKey)
    ) {
      this.props.dispatch(createTableSql());
    }
    // CLEVER WARNING sneaky if, the above all can dispatch errors
  }

  render() {
    const {
      columns,
      primaryKeys,
      allSchemas,
      foreignKeys,
      uniqueKeys,
      fkToggled,
      tableName,
      currentSchema,
      dispatch,
      ongoingRequest,
      lastError,
      lastSuccess,
      internalError,
      dataTypes,
      schemaList,
      columnDefaultFunctions,
      columnTypeCasts,
    } = this.props;
    const styles = require('../../../Common/TableCommon/Table.scss');
    const getCreateBtnText = () => {
      let createBtnText = 'Add Table';
      if (ongoingRequest) {
        createBtnText = 'Creating...';
      } else if (lastError) {
        createBtnText = 'Creating Failed. Try again';
      } else if (internalError) {
        createBtnText = 'Creating Failed. Try again';
      } else if (lastSuccess) {
        createBtnText = 'Created! Redirecting...';
      }
      return createBtnText;
    };

    return (
      <div
        className={`${styles.addTablesBody} ${styles.clear_fix} ${
          styles.padd_left
        }`}
      >
        <Helmet title="Add Table - Data | Hasura" />
        <div className={styles.subHeader}>
          <h2 className={styles.heading_text}>Add a new table</h2>
          <div className="clearfix" />
        </div>
        <br />
        <div className={`container-fluid ${styles.padd_left_remove}`}>
          <div
            className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}
          >
            <TableName onChange={this.onTableNameChange.bind(this)} />
            <hr />
            <TableColumns
              uniqueKeys={uniqueKeys}
              dataTypes={dataTypes}
              columnDefaultFunctions={columnDefaultFunctions}
              columnTypeCasts={columnTypeCasts}
              columns={columns}
              onRemoveColumn={this.onRemoveColumn}
              onColumnChange={this.onColumnNameChange}
              onColTypeChange={this.onColTypeChange}
              onColNullableChange={this.onColNullableChange}
              onColUniqueChange={this.onColUniqueChange}
              setColDefaultValue={this.setColDefaultValue}
            />
            <hr />
            <h4 className={styles.subheading_text}>
              Primary Key &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.primaryKeyDescription}
              >
                <i
                  className={`fa fa-question-circle ${styles.iClickable}`}
                  aria-hidden="true"
                />
              </OverlayTrigger>{' '}
              &nbsp; &nbsp;
            </h4>
            <PrimaryKeySelector
              primaryKeys={primaryKeys}
              columns={columns}
              setPk={setPk}
              dispatch={dispatch}
            />
            <hr />
            <h4 className={styles.subheading_text}>
              Foreign Keys &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.foreignKeyDescription}
              >
                <i
                  className={`fa fa-question-circle ${styles.iClickable}`}
                  aria-hidden="true"
                />
              </OverlayTrigger>{' '}
              &nbsp; &nbsp;
            </h4>
            <ForeignKeyWrapper
              allSchemas={allSchemas}
              columns={columns}
              currentSchema={currentSchema}
              tableName={tableName}
              foreignKeys={foreignKeys}
              dispatch={dispatch}
              setForeignKeys={setForeignKeys}
              fkToggled={fkToggled}
              schemaList={schemaList}
            />
            <hr />
            <h4 className={styles.subheading_text}>
              Unique Keys &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.uniqueKeyDescription}
              >
                <i
                  className={`fa fa-question-circle ${styles.iClickable}`}
                  aria-hidden="true"
                />
              </OverlayTrigger>{' '}
              &nbsp; &nbsp;
            </h4>
            <UniqueKeyWrapper
              allSchemas={allSchemas}
              columns={columns}
              currentSchema={currentSchema}
              tableName={tableName}
              uniqueKeys={uniqueKeys}
              dispatch={dispatch}
              setUniqueKeys={setUniqueKeys}
            />
            <hr />
            <TableComment onChange={this.onTableCommentChange} />
            <hr />
            <Button
              type="submit"
              onClick={this.validateAndSubmit}
              data-test="table-create"
              color="yellow"
              size="sm"
            >
              {getCreateBtnText()}
            </Button>
          </div>
        </div>
      </div>
    );
  }
}

AddTable.propTypes = {
  columns: PropTypes.array.isRequired,
  tableName: PropTypes.string,
  allSchemas: PropTypes.array.isRequired,
  primaryKeys: PropTypes.array.isRequired,
  foreignKeys: PropTypes.array.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  internalError: PropTypes.string,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
  ...state.addTable.table,
  allSchemas: state.tables.allSchemas,
  currentSchema: state.tables.currentSchema,
  dataTypes: state.tables.columnDataTypes,
  columnDefaultFunctions: state.tables.columnDefaultFunctions,
  columnTypeCasts: state.tables.columnTypeCasts,
  columnDataTypeFetchErr: state.tables.columnDataTypeFetchErr,
  schemaList: state.tables.schemaList,
});

const addTableConnector = connect => connect(mapStateToProps)(AddTable);

export default addTableConnector;
