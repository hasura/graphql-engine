import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { Link } from 'react-router';
import { appPrefix } from '../push';
import TableHeader from '../TableCommon/TableHeader';
import {
  fkRefTableChange,
  fkLColChange,
  fkRColChange,
  toggleFKCheckBox,
  saveColChangesWithFkSql,
  isColumnUnique,
  deleteTableSql,
  deleteConstraintSql,
  addColSql,
  untrackTableSql,
  RESET,
  TOGGLE_ACTIVE_COLUMN,
  saveColumnChangesSql,
  deleteColumnSql,
} from '../TableModify/ModifyActions';
import { ordinalColSort } from '../utils';
import dataTypes from '../Common/DataTypes';
import {
  convertListToDictUsingKV,
  convertListToDict,
} from '../../../../utils/data';
import { setTable } from '../DataActions';
import { showErrorNotification } from '../Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';

const alterTypeOptions = dataTypes.map((datatype, index) => (
  <option value={datatype.value} key={index} title={datatype.description}>
    {datatype.name}
  </option>
));

const ColumnEditor = ({
  column,
  onSubmit,
  onDelete,
  allSchemas,
  fkAdd,
  tableName,
  dispatch,
}) => {
  //  eslint-disable-line no-unused-vars
  const c = column;
  const styles = require('./Modify.scss');
  let [inullable, iunique, idefault, itype] = [null, null, null, null];
  // NOTE: the datatypes is filtered of serial and bigserial where hasuraDatatype === null
  const refTable = fkAdd.refTable;
  const tableSchema = allSchemas.find(t => t.table_name === tableName);
  const rcol = fkAdd.rcol;
  const typeMap = convertListToDictUsingKV(
    'hasuraDatatype',
    'value',
    dataTypes.filter(dataType => dataType.hasuraDatatype)
  );
  const refSchema = allSchemas.find(t => t.table_name === refTable);
  // const allTableNamesExceptCurrent = allSchemas.filter(t => t.table_name !== tableName);
  const allTableNames = allSchemas.map(t => t.table_name);
  allTableNames.sort();

  const refColumnNames = refSchema
    ? refSchema.columns.map(col => col.column_name)
    : [];
  refColumnNames.sort();
  const onFKRefTableChange = e => {
    dispatch(fkRefTableChange(e.target.value));
  };
  const onFKRefColumnChange = e => {
    dispatch(fkRColChange(e.target.value));
  };
  const checkExistingForeignKey = () => {
    const numFk = tableSchema.foreign_key_constraints.length;
    let fkName = '';
    const onDeleteFK = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(deleteConstraintSql(tableName, fkName));
      }
    };
    if (numFk > 0) {
      for (let i = 0; i < numFk; i++) {
        const fk = tableSchema.foreign_key_constraints[i];
        if (
          Object.keys(fk.column_mapping).toString() === c.column_name.toString()
        ) {
          fkName = fk.constraint_name;
          return (
            <div className={`${styles.display_flex} form-group`}>
              <label className="col-xs-3 text-right">Foreign Key</label>
              <div className="col-xs-9">
                <h5>
                  <span>{fk.ref_table} :: </span>
                  <span className={styles.add_mar_right}>
                    {Object.keys(fk.column_mapping)
                      .map(l => fk.column_mapping[l])
                      .join(',')}
                  </span>
                  <Link
                    to={`${appPrefix}/schema/tables/${tableName}/relationships`}
                  >
                    <button
                      className={`${styles.default_button} btn`}
                      type="button"
                    >
                      +Add relationship
                    </button>
                  </Link>
                  &nbsp;
                  <button
                    className="btn btn-danger btn-sm"
                    onClick={onDeleteFK}
                  >
                    {' '}
                    Remove Constraint{' '}
                  </button>{' '}
                  &nbsp;
                </h5>
              </div>
            </div>
          );
        }
      }
    }
    return (
      <div className={`${styles.display_flex} form-group`}>
        <label className="col-xs-3 text-right">
          <input
            type="checkbox"
            checked={fkAdd.fkCheckBox}
            onChange={e => {
              dispatch(toggleFKCheckBox(e.target.checked));
            }}
            value="ForeignKey"
            data-test="foreign-key-checkbox"
          />{' '}
          Foreign Key
        </label>
        <div className="col-xs-6">
          <select
            className={`${styles.fkSelect} ${styles.fkInEdit} ${
              styles.fkInEditLeft
            } input-sm form-control`}
            disabled={fkAdd.fkCheckBox === false}
            value={refTable}
            onChange={onFKRefTableChange}
          >
            <option disabled value="">
              Reference table
            </option>
            {allTableNames.map((tName, i) => (
              <option key={i} value={tName}>
                {tName}
              </option>
            ))}
          </select>
          <select
            className={`${styles.fkSelect} ${
              styles.fkInEdit
            } input-sm form-control`}
            disabled={fkAdd.fkCheckBox === false}
            value={rcol}
            onChange={onFKRefColumnChange}
          >
            <option disabled value="">
              Reference column
            </option>
            {refColumnNames.map((co, i) => (
              <option key={i} value={co}>
                {co}
              </option>
            ))}
          </select>
        </div>
      </div>
    );
  };
  let isPrimaryKey = false;
  const isUnique = isColumnUnique(tableSchema, c.column_name);

  if (
    tableSchema.primary_key &&
    tableSchema.primary_key.columns.includes(c.column_name)
  ) {
    isPrimaryKey = true;
  }

  const additionalOptions = [];
  let finalDefaultValue = typeMap[c.data_type];
  if (!typeMap[c.data_type]) {
    finalDefaultValue = c.data_type;
    additionalOptions.push(
      <option value={finalDefaultValue} key={finalDefaultValue}>
        {c.data_type}
      </option>
    );
  }
  return (
    <div className={`${styles.colEditor} container-fluid`}>
      <form
        className="form-horizontal"
        onSubmit={e => {
          e.preventDefault();
          onSubmit(
            itype.value,
            inullable.value,
            iunique.value,
            idefault.value,
            column
          );
        }}
      >
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Type</label>
          <div className="col-xs-6">
            <select
              ref={n => (itype = n)}
              className="input-sm form-control"
              defaultValue={finalDefaultValue}
              disabled={isPrimaryKey}
            >
              {alterTypeOptions}
              {additionalOptions}
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Nullable</label>
          <div className="col-xs-6">
            <select
              ref={n => (inullable = n)}
              className="input-sm form-control"
              defaultValue={c.is_nullable === 'NO' ? 'false' : 'true'}
              disabled={isPrimaryKey}
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Unique</label>
          <div className="col-xs-6">
            <select
              ref={n => (iunique = n)}
              className="input-sm form-control"
              defaultValue={isUnique.toString()}
              disabled={isPrimaryKey}
            >
              <option value="true">True</option>
              <option value="false">False</option>
            </select>
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Default</label>
          <div className="col-xs-6">
            <input
              ref={n => (idefault = n)}
              className="input-sm form-control"
              defaultValue={c.column_default ? c.column_default : null}
              type="text"
              disabled={isPrimaryKey}
            />
          </div>
        </div>
        {checkExistingForeignKey()}
        <div className="row">
          <button type="submit" className={`${styles.yellow_button} btn`}>
            Save
          </button>
          {!isPrimaryKey ? (
            <button
              type="submit"
              className={`${styles.yellow_button1} btn btn-danger btn-sm`}
              onClick={e => {
                e.preventDefault();
                onDelete();
              }}
            >
              Remove
            </button>
          ) : null}
        </div>
      </form>
      <div className="row">
        <br />
      </div>
    </div>
  );
};

class ModifyTable extends Component {
  componentDidMount() {
    this.props.dispatch({ type: RESET });

    this.props.dispatch(setTable(this.props.tableName));
  }

  render() {
    const {
      tableName,
      allSchemas,
      dispatch,
      activeEdit,
      fkAdd,
      migrationMode,
      currentSchema,
    } = this.props;
    const styles = require('./Modify.scss');
    const tableSchema = allSchemas.find(t => t.table_name === tableName);
    const hasPrimaryKeys =
      tableSchema.primary_key && tableSchema.primary_key.columns.length > 0;
    const primaryKeyDict = hasPrimaryKeys
      ? convertListToDict(tableSchema.primary_key.columns)
      : {};

    const columns = tableSchema.columns.sort(ordinalColSort);
    const columnEditors = columns.map((c, i) => {
      let btnText = 'Edit';
      let colEditor = null;
      let bg = '';
      const colName = c.column_name;
      const onSubmit = (type, nullable, unique, def, column) => {
        // dispatch(saveColumnChangesSql(tableName, colName, type, nullable, def, column));
        if (fkAdd.fkCheckBox === true) {
          dispatch(fkLColChange(column.column_name));
          dispatch(
            saveColChangesWithFkSql(
              tableName,
              colName,
              type,
              nullable,
              unique,
              def,
              column
            )
          );
        } else {
          dispatch(
            saveColumnChangesSql(
              tableName,
              colName,
              type,
              nullable,
              unique,
              def,
              column
            )
          );
        }
      };
      const onDelete = () => {
        const isOk = confirm('Are you sure you want to delete?');
        if (isOk) {
          dispatch(deleteColumnSql(tableName, colName, c));
        }
      };
      if (activeEdit.column === colName) {
        btnText = 'Close';
        if (primaryKeyDict[colName]) {
          colEditor = (
            <ColumnEditor
              column={c}
              onSubmit={onSubmit}
              onDelete={() => {
                const isOk = window.confirm(`Are you sure? Deleting a primary key DISABLE ALL ROW EDIT VIA THE CONSOLE.
              Also, this will delete everything associated with the column (included related entities in other tables) permanently?`);
                if (isOk) {
                  dispatch(deleteColumnSql(tableName, colName));
                }
              }}
              fkAdd={fkAdd}
              tableName={tableName}
              dispatch={dispatch}
              allSchemas={allSchemas}
            />
          );
        } else {
          colEditor = (
            <ColumnEditor
              column={c}
              onSubmit={onSubmit}
              onDelete={onDelete}
              fkAdd={fkAdd}
              tableName={tableName}
              dispatch={dispatch}
              allSchemas={allSchemas}
            />
          );
        }
        bg = styles.activeEdit;
      }
      // check if column name is primary key
      let isPrimaryKey = false;
      if (hasPrimaryKeys) {
        if (tableSchema.primary_key.columns.includes(colName)) {
          isPrimaryKey = true;
        }
      }
      let isForeignKey = false;
      if (tableSchema.foreign_key_constraints.length > 0) {
        const numFk = tableSchema.foreign_key_constraints.length;
        for (let iFk = 0; iFk < numFk; iFk++) {
          const fk = tableSchema.foreign_key_constraints[iFk];
          if (
            Object.keys(fk.column_mapping).toString() ===
            c.column_name.toString()
          ) {
            isForeignKey = true;
            break;
          }
        }
      }

      const keyProperties = () => {
        if (isPrimaryKey && isForeignKey) {
          return <i> - primary key, foreign key </i>;
        }
        if (isPrimaryKey) {
          return <i> - primary key </i>;
        }
        if (isForeignKey) {
          return <i> - foreign key </i>;
        }
        return <i> {''} </i>;
      };
      return (
        <div key={i} className={bg}>
          <div className="container-fluid">
            <div className="row">
              <h5 className={styles.padd_bottom}>
                <button
                  className={`${styles.add_mar_small} btn btn-xs btn-default`}
                  onClick={() => {
                    dispatch({ type: TOGGLE_ACTIVE_COLUMN, column: colName });
                  }}
                >
                  {btnText}
                </button>
                <b>{colName}</b> {keyProperties()}
                &nbsp;
              </h5>
              {colEditor}
            </div>
          </div>
        </div>
      );
    });

    let colNameInput;
    let colTypeInput;
    let colNullInput;
    let colUniqueInput;
    let colDefaultInput;

    // check if platform version is >= 0.15.33 and show untrack
    const untrackBtn = (
      <button
        type="submit"
        className={`${styles.add_mar_right} btn btn-sm btn-default`}
        onClick={() => {
          const isOk = confirm('Are you sure to untrack?');
          if (isOk) {
            dispatch(untrackTableSql(tableName));
          }
        }}
      >
        Untrack Table
      </button>
    );

    // if (tableSchema.primary_key.columns > 0) {}
    return (
      <div className={`${styles.container} container-fluid`}>
        <TableHeader
          dispatch={dispatch}
          tableName={tableName}
          tabName="modify"
          migrationMode={migrationMode}
          currentSchema={currentSchema}
        />
        <br />
        <div className={`container-fluid ${styles.padd_left_remove}`}>
          <div className={`col-xs-9 ${styles.padd_left_remove}`}>
            <h4 className={styles.subheading_text}>Columns</h4>
            {columnEditors}
            <div className={styles.activeEdit}>
              <form
                className={`form-inline ${styles.display_flex}`}
                onSubmit={e => {
                  e.preventDefault();
                  // validate before sending
                  if (!gqlPattern.test(colNameInput.value)) {
                    dispatch(
                      showErrorNotification(
                        gqlColumnErrorNotif[0],
                        gqlColumnErrorNotif[1],
                        gqlColumnErrorNotif[2],
                        gqlColumnErrorNotif[3]
                      )
                    );
                  } else if (
                    colNameInput.value === '' ||
                    colTypeInput.value === ''
                  ) {
                    dispatch(
                      showErrorNotification(
                        'Error creating column!',
                        'Column name/type cannot be empty',
                        '',
                        {
                          custom: 'Column name/type cannot be empty',
                        }
                      )
                    );
                  } else {
                    dispatch(
                      addColSql(
                        tableName,
                        colNameInput.value,
                        colTypeInput.value,
                        colNullInput.checked,
                        colUniqueInput.checked,
                        colDefaultInput.value,
                        e.target
                      )
                    );
                  }
                }}
              >
                <input
                  placeholder="column name"
                  type="text"
                  className={`${styles.input} input-sm form-control`}
                  ref={n => (colNameInput = n)}
                />
                <select
                  className={`${styles.select} input-sm form-control`}
                  defaultValue=""
                  ref={n => (colTypeInput = n)}
                  data-test="data-type"
                >
                  <option disabled value="">
                    -- type --
                  </option>
                  {alterTypeOptions}
                </select>
                <input
                  type="checkbox"
                  defaultChecked
                  className={`${styles.input} ${
                    styles.nullable
                  } input-sm form-control`}
                  ref={n => (colNullInput = n)}
                  data-test="nullable-checkbox"
                />
                <label className={styles.nullLabel}>Nullable</label>
                <input
                  type="checkbox"
                  className={`${styles.input} ${
                    styles.nullable
                  } input-sm form-control`}
                  ref={n => (colUniqueInput = n)}
                  data-test="unique-checkbox"
                />
                <label className={styles.nullLabel}>Unique</label>
                <input
                  placeholder="default value"
                  type="text"
                  className={`${styles.input} ${
                    styles.defaultInput
                  } input-sm form-control`}
                  ref={n => (colDefaultInput = n)}
                />
                <button type="submit" className="btn btn-sm btn-warning">
                  + Add column
                </button>
              </form>
            </div>
            <hr />
            {untrackBtn}
            <button
              type="submit"
              className="btn btn-sm btn-danger"
              onClick={() => {
                const isOk = confirm('Are you sure?');
                if (isOk) {
                  dispatch(deleteTableSql(tableName, tableSchema));
                }
              }}
            >
              Delete table
            </button>
            <br />
            <br />
          </div>
        </div>
        <div className={`hidden col-xs-2 ${styles.fixed}`}>{alert}</div>
      </div>
    );
  }
}

ModifyTable.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  activeEdit: PropTypes.object.isRequired,
  fkAdd: PropTypes.object.isRequired,
  relAdd: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  allSchemas: state.tables.allSchemas,
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
