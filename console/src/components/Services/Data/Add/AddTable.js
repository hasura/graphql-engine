import PropTypes from 'prop-types';
import React, { Component } from 'react';
import Helmet from 'react-helmet';

import * as tooltip from './Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import dataTypes from '../Common/DataTypes';
import { showErrorNotification } from '../Notification';

import {
  setTableName,
  setTableComment,
  removeColumn,
  setColName,
  setColType,
  setColNullable,
  setColDefault,
  setColUnique,
  removeColDefault,
  addCol,
} from './AddActions';
import {
  setDefaults,
  setPk,
  addPk,
  removePk,
  createTableSql,
} from './AddActions';
import { validationError, resetValidation } from './AddActions';

import {
  ATLEAST_ONE_PRIMARY_KEY_MSG,
  ATLEAST_ONE_COLUMN_MSG,
} from './AddWarning';
import { primaryKeyAlreadyPresentMsg, fieldRepeatedMsg } from './AddWarning';

import {
  listDulplicate,
  // convertListToDictUsingKV,
} from '../../../../utils/data';

import gqlPattern, {
  gqlTableErrorNotif,
  gqlColumnErrorNotif,
} from '../Common/GraphQLValidation';
/*
const typeDescriptionDict = convertListToDictUsingKV(
  'value',
  'description',
  dataTypes
);
*/
class AddTable extends Component {
  constructor(props) {
    super(props);
    this.props.dispatch(setDefaults());
    const { columns, dispatch } = this.props;
    columns.map((column, i) => {
      //eslint-disable-line
      let defValue = '';
      if ('default' in column) {
        defValue = column.default.value;
      }
      if (defValue === '') {
        dispatch(removeColDefault(i));
      }
    });
  }

  componentWillUnmount() {
    this.props.dispatch(setDefaults());
  }
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
      const listOfRepeats = listDulplicate(
        this.props.columns.map(column => column.name)
      );
      if (listOfRepeats.length > 0) {
        this.props.dispatch(validationError(fieldRepeatedMsg(listOfRepeats)));
        return false;
      }
      // check for column value being valid graphql
      let isValid = true;
      this.props.columns.filter(c => c.name !== '').map(c => {
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

  primaryKeyValidation(i, e) {
    const value = parseInt(e.target.value, 10);
    this.props.dispatch(resetValidation());
    if (this.props.primaryKeys.filter(key => value === key).length > 0) {
      this.props.dispatch(
        validationError(
          primaryKeyAlreadyPresentMsg(this.props.columns[value].name)
        )
      );
      return false;
    }

    this.props.dispatch(setPk(value, i));
    if (i + 1 === this.props.primaryKeys.length) {
      this.props.dispatch(addPk());
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
      dispatch,
      ongoingRequest,
      lastError,
      lastSuccess,
      internalError,
    } = this.props;
    const styles = require('../TableCommon/Table.scss');
    const cols = columns.map((column, i) => {
      let removeIcon;
      if (i + 1 === columns.length) {
        removeIcon = <i className={`${styles.fontAwosomeClose}`} />;
      } else {
        removeIcon = (
          <i
            className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
            onClick={() => {
              dispatch(removeColumn(i));
            }}
          />
        );
      }
      let defValue = '';
      if ('default' in column) {
        defValue = column.default.value;
      }
      let defPlaceholder = 'default_value';
      if (column.type === 'timestamptz') {
        defPlaceholder = 'example: now()';
      } else if (column.type === 'date') {
        defPlaceholder = '';
      } else if (column.type === 'uuid') {
        defPlaceholder = 'example: gen_random_uuid()';
      }
      return (
        <div key={i} className={`${styles.display_flex} form-group`}>
          <input
            type="text"
            className={`${styles.input} form-control ${styles.add_mar_right}`}
            value={column.name}
            placeholder="column_name"
            onChange={e => {
              dispatch(
                setColName(e.target.value, i, this.refs[`nullable${i}`].checked)
              );
            }}
            data-test={`column-${i}`}
          />
          <select
            value={column.type}
            className={`${styles.select} ${styles.selectWidth} form-control ${
              styles.add_pad_left
            }`}
            onChange={e => {
              dispatch(
                setColType(e.target.value, i, this.refs[`nullable${i}`].checked)
              );
              if (i + 1 === columns.length) {
                dispatch(addCol());
              }
            }}
            data-test={`col-type-${i}`}
          >
            {column.type === '' ? (
              <option disabled value="">
                -- type --
              </option>
            ) : null}
            {/* The below makes a set of options based of the available datatype. Refer Common/Datatypes.js for more info. */}
            {dataTypes.map((datatype, index) => (
              <option
                value={datatype.value}
                key={index}
                title={datatype.description}
              >
                {datatype.name}
              </option>
            ))}
          </select>
          {/*
          {typeDescriptionDict && typeDescriptionDict[column.type] ? (
            <span>
              &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.dataTypeDescription(
                  typeDescriptionDict[column.type]
                )}
              >
                <i className="fa fa-question-circle" aria-hidden="true" />
              </OverlayTrigger>{' '}
              &nbsp; &nbsp;
            </span>
          ) : null}
          */}
          <input
            placeholder={defPlaceholder}
            type="text"
            value={defValue}
            className={`${styles.inputDefault} ${
              styles.defaultWidth
            } form-control ${styles.add_pad_left}`}
            onChange={e => {
              dispatch(
                setColDefault(
                  e.target.value,
                  i,
                  this.refs[`nullable${i}`].checked
                )
              );
            }}
            data-test={`col-default-${i}`}
          />{' '}
          <input
            className={`${styles.inputCheckbox} form-control `}
            checked={columns[i].nullable}
            type="checkbox"
            ref={`nullable${i}`}
            onChange={e => {
              dispatch(setColNullable(e.target.checked, i));
            }}
            data-test={`nullable-${i}`}
          />{' '}
          <label>Nullable</label>
          <input
            className={`${styles.inputCheckbox} form-control `}
            checked={columns[i].unique}
            type="checkbox"
            ref={`unique${i}`}
            onChange={e => {
              dispatch(setColUnique(e.target.checked, i));
            }}
            data-test={`unique-${i.toString()}`}
          />{' '}
          <label>Unique</label>
          {removeIcon}
        </div>
      );
    });
    const pks = primaryKeys.map((pk, i) => {
      let removeIcon;
      if (i + 1 === primaryKeys.length) {
        removeIcon = null;
      } else {
        removeIcon = (
          <i
            className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
            onClick={() => {
              dispatch(removePk(i));
            }}
          />
        );
      }
      return (
        <div key={i} className="form-group">
          <select
            value={pk || ''}
            className={`${styles.select} form-control ${styles.add_pad_left}`}
            onChange={this.primaryKeyValidation.bind(this, i)}
            data-test={`primary-key-select-${i}`}
            data-test={`primary-key-select-${i.toString()}`}
          >
            {pk === '' ? (
              <option disabled value="">
                -- select --
              </option>
            ) : null}
            {columns.map(({ name }, j) => (
              <option key={j} value={j}>
                {name}
              </option>
            ))}
          </select>
          {removeIcon}
        </div>
      );
    });
    let createBtnText = 'Create';
    if (ongoingRequest) {
      createBtnText = 'Creating...';
    } else if (lastError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (internalError) {
      createBtnText = 'Creating Failed. Try again';
    } else if (lastSuccess) {
      createBtnText = 'Created! Redirecting...';
    }

    return (
      <div
        className={`${styles.addTablesBody} ${styles.main_wrapper} ${
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
            <h4 className={styles.subheading_text}>Table Name &nbsp; &nbsp;</h4>
            <input
              type="text"
              data-test="tableName"
              placeholder="table_name"
              className={`${styles.tableNameInput} form-control`}
              onChange={e => {
                dispatch(setTableName(e.target.value));
              }}
            />
            <hr />
            <h4 className={styles.subheading_text}>Columns</h4>
            {cols}
            <hr />
            <h4 className={styles.subheading_text}>
              Primary Key &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.primaryKeyDescription}
              >
                <i className="fa fa-question-circle" aria-hidden="true" />
              </OverlayTrigger>{' '}
              &nbsp; &nbsp;
            </h4>
            {pks}
            <hr />
            <h4 className={styles.subheading_text}>Comment &nbsp; &nbsp;</h4>
            <input
              type="text"
              data-test="tableComment"
              placeholder="comment"
              className={`${styles.tableNameInput} form-control`}
              onChange={e => {
                dispatch(setTableComment(e.target.value));
              }}
            />
            <hr />
            <button
              type="submit"
              className={`btn ${styles.yellow_button}`}
              onClick={this.submitValidation.bind(this)}
              data-test="table-create"
            >
              {createBtnText}
            </button>
          </div>
        </div>
      </div>
    );
  }
}

AddTable.propTypes = {
  columns: PropTypes.array.isRequired,
  tableName: PropTypes.string,
  primaryKeys: PropTypes.array.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  internalError: PropTypes.string,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({ ...state.addTable.table });

const addTableConnector = connect => connect(mapStateToProps)(AddTable);

export default addTableConnector;
