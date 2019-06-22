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
  addCol,
  setUniqueKeys,
  setFreqUsedColumn,
} from './AddActions';

import { fetchColumnTypeInfo, RESET_COLUMN_TYPE_INFO } from '../DataActions';
import { setDefaults, setPk, createTableSql } from './AddActions';
import { validationError, resetValidation } from './AddActions';

import {
  ATLEAST_ONE_PRIMARY_KEY_MSG,
  ATLEAST_ONE_COLUMN_MSG,
  fieldRepeatedMsg,
} from './AddWarning';

import {
  listDuplicate,
  // convertListToDictUsingKV,
} from '../../../../utils/data';

import gqlPattern, {
  gqlTableErrorNotif,
  gqlColumnErrorNotif,
} from '../Common/GraphQLValidation';

import styles from '../../../Common/TableCommon/Table.scss';
import { frequentlyUsedColumns, getFrequentlyUsedColumn } from './utils';
import Dropdown from '../../../Common/Dropdown/Dropdown';
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
    const { dispatch, columns } = this.props;
    dispatch(setColType(value, i));
    if (i + 1 === columns.length) {
      dispatch(addCol());
    }
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

  columnValidation() {
    if (this.props.columns.length <= 0) {
      // this.props.dispatch(validationError(ATLEAST_ONE_COLUMN_MSG));
      // alert(ATLEAST_ONE_COLUMN_MSG);
      this.props.dispatch(
        showErrorNotification(
          'Error creating table!',
          'Minimum one column required',
          '',
          {
            custom: ATLEAST_ONE_COLUMN_MSG,
          }
        )
      );
      return false;
    } else if (this.props.columns.length === 1) {
      // check if name and type is not empty
      if (
        this.props.columns[0].name === '' ||
        this.props.columns[0].type === ''
      ) {
        // this.props.dispatch(validationError(ATLEAST_ONE_COLUMN_MSG));
        // alert(ATLEAST_ONE_COLUMN_MSG);
        this.props.dispatch(
          showErrorNotification(
            'Error creating table!',
            'Column name cannot be empty',
            '',
            {
              custom: 'Column name cannot be empty',
            }
          )
        );
        return false;
      } else if (!gqlPattern.test(this.props.columns[0].name)) {
        this.props.dispatch(
          showErrorNotification(
            gqlColumnErrorNotif[0],
            gqlColumnErrorNotif[1],
            gqlColumnErrorNotif[2],
            gqlColumnErrorNotif[3]
          )
        );
        return false;
      }
    } else if (this.props.columns.length > 1) {
      // check for repeatition
      const listOfRepeats = listDuplicate(
        this.props.columns.map(column => column.name)
      );
      if (listOfRepeats.length > 0) {
        this.props.dispatch(validationError(fieldRepeatedMsg(listOfRepeats)));
        return false;
      }
      // check for column value being valid graphql
      let isValid = true;
      this.props.columns
        .filter(c => c.name !== '')
        .map(c => {
          if (!gqlPattern.test(c.name)) {
            this.props.dispatch(
              showErrorNotification(
                gqlColumnErrorNotif[0],
                gqlColumnErrorNotif[1],
                gqlColumnErrorNotif[2],
                gqlColumnErrorNotif[3]
              )
            );
            isValid = false;
          }
        });
      if (!isValid) {
        return false;
      }
    }
    return true;
  }

  minPrimaryKeyCheck() {
    if (this.props.primaryKeys.filter(key => key !== '').length <= 0) {
      // this.props.dispatch(validationError(ATLEAST_ONE_PRIMARY_KEY_MSG));
      // alert(ATLEAST_ONE_PRIMARY_KEY_MSG);
      this.props.dispatch(
        showErrorNotification(
          'Error creating table!',
          'Select atleast one primary key',
          '',
          {
            custom: ATLEAST_ONE_PRIMARY_KEY_MSG,
          }
        )
      );
      return false;
    }
    return true;
  }

  tableNameCheck() {
    if (this.props.tableName === '' || this.props.tableName === null) {
      // this.props.dispatch(validationError('Table name cannot be empty'));
      // alert('Table name cannot be empty');
      this.props.dispatch(
        showErrorNotification(
          'Error creating table!',
          'Table name cannot be empty',
          '',
          {
            custom: 'Table name cannot be empty. Please add a name',
          }
        )
      );
      return false;
    } else if (!gqlPattern.test(this.props.tableName)) {
      this.props.dispatch(
        showErrorNotification(
          gqlTableErrorNotif[0],
          gqlTableErrorNotif[1],
          gqlTableErrorNotif[2],
          gqlTableErrorNotif[3]
        )
      );
      return false;
    }
    return true;
  }

  submitValidation() {
    this.props.dispatch(resetValidation());
    // table name validation
    if (this.tableNameCheck()) {
      // column validation.
      if (this.columnValidation()) {
        // The primary key validation ensure.
        if (this.minPrimaryKeyCheck()) {
          this.props.dispatch(createTableSql());
        }
      }
    }
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

    const frequentlyUsedColumnsOptions = () => {
      return frequentlyUsedColumns.map(fuc => {
        const { title, subTitle } = getFrequentlyUsedColumn(fuc);
        return {
          content: (
            <div>
              <div>
                <b>{title}</b>
              </div>
              <div>{subTitle}</div>
            </div>
          ),
          onClick: () => dispatch(setFreqUsedColumn(fuc)),
        };
      });
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
            <div>
              <Dropdown
                testId={'frequently-used-columns'}
                options={frequentlyUsedColumnsOptions()}
                position="bottom"
                key={'frequently-used-columns'}
                keyPrefix={'frequently-used-columns'}
              >
                <Button color="white" size="sm">
                  + Frequently used columns
                </Button>
              </Dropdown>
            </div>
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
              onClick={this.submitValidation.bind(this)}
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
