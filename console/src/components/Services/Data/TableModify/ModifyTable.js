import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { Link } from 'react-router';
import TableHeader from '../TableCommon/TableHeader';
import {
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
} from './ModifyActions';
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
import {
  setTable,
  fetchTableComment,
  fetchColumnComment,
} from '../DataActions';
import { showErrorNotification } from '../Notification';
import gqlPattern, { gqlColumnErrorNotif } from '../Common/GraphQLValidation';
import {
  INTEGER,
  SERIAL,
  BIGINT,
  BIGSERIAL,
  UUID,
  JSON,
  JSONB,
  TIMESTAMP,
  TIME,
} from '../../../../constants';
import Button from '../../Layout/Button/Button';

const appPrefix = '/data';

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
  currentSchema,
  columnComment,
}) => {
  //  eslint-disable-line no-unused-vars
  const c = column;
  const styles = require('./Modify.scss');
  let [inullable, iunique, idefault, icomment, itype] = [
    null,
    null,
    null,
    null,
    null,
  ];
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
                    to={`${appPrefix}/schema/${currentSchema}/tables/${tableName}/relationships`}
                  >
                    <Button
                      color="white"
                      size="sm"
                      type="button"
                      data-test="add-rel-mod"
                    >
                      +Add relationship
                    </Button>
                  </Link>
                  &nbsp;
                  <Button
                    color="red"
                    size="sm"
                    onClick={onDeleteFK}
                    data-test="remove-constraint-button"
                  >
                    {' '}
                    Remove Constraint{' '}
                  </Button>{' '}
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
            data-test="ref-table"
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
            data-test="ref-col"
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

  const generateAlterOptions = datatypeOptions => {
    return dataTypes.map(datatype => {
      if (datatypeOptions.includes(datatype.value)) {
        return (
          <option
            value={datatype.value}
            key={datatype.name}
            title={datatype.description}
          >
            {datatype.name}
          </option>
        );
      }
    });
  };

  const modifyAlterOptions = columntype => {
    const integerOptions = [
      'integer',
      'serial',
      'bigint',
      'bigserial',
      'numeric',
      'text',
    ];
    const bigintOptions = ['bigint', 'bigserial', 'text', 'numeric'];
    const uuidOptions = ['uuid', 'text'];
    const jsonOptions = ['json', 'jsonb', 'text'];
    const timestampOptions = ['timestamptz', 'text'];
    const timeOptions = ['timetz', 'text'];
    switch (columntype) {
      case INTEGER:
        return generateAlterOptions(integerOptions);

      case SERIAL:
        return generateAlterOptions(integerOptions);

      case BIGINT:
        return generateAlterOptions(bigintOptions);

      case BIGSERIAL:
        return generateAlterOptions(bigintOptions);

      case UUID:
        return generateAlterOptions(uuidOptions);

      case JSON:
        return generateAlterOptions(jsonOptions);

      case JSONB:
        return generateAlterOptions(jsonOptions);

      case TIMESTAMP:
        return generateAlterOptions(timestampOptions);

      case TIME:
        return generateAlterOptions(timeOptions);

      default:
        return generateAlterOptions([columntype, 'text']);
    }
  };

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
            icomment.value,
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
              {modifyAlterOptions(column.data_type)}
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
              data-test="edit-col-nullable"
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
              data-test="edit-col-unique"
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
              data-test="edit-col-default"
            />
          </div>
        </div>
        <div className={`${styles.display_flex} form-group`}>
          <label className="col-xs-3 text-right">Comment</label>
          <div className="col-xs-6">
            <input
              ref={n => (icomment = n)}
              className="input-sm form-control"
              defaultValue={columnComment ? columnComment.result[1] : null}
              type="text"
              data-test="edit-col-comment"
            />
          </div>
        </div>
        {checkExistingForeignKey()}
        <div className="row">
          <Button
            type="submit"
            color="yellow"
            className={styles.button_mar_right}
            size="sm"
            data-test="save-button"
          >
            Save
          </Button>
          {!isPrimaryKey ? (
            <Button
              type="submit"
              color="red"
              size="sm"
              onClick={e => {
                e.preventDefault();
                onDelete();
              }}
              data-test="remove-button"
            >
              Remove
            </Button>
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
    const { dispatch } = this.props;
    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    dispatch(fetchTableComment(this.props.tableName));
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
      tableComment,
      columnComment,
      tableCommentEdit,
    } = this.props;
    const styles = require('./Modify.scss');
    const tableSchema = allSchemas.find(t => t.table_name === tableName);
    const hasPrimaryKeys =
      tableSchema &&
      tableSchema.primary_key &&
      tableSchema.primary_key.columns.length > 0;
    const primaryKeyDict = hasPrimaryKeys
      ? convertListToDict(tableSchema.primary_key.columns)
      : {};

    const columns = tableSchema.columns.sort(ordinalColSort);
    const columnEditors = columns.map((c, i) => {
      let btnText = 'Edit';
      let colEditor = null;
      let bg = '';
      const colName = c.column_name;
      const onSubmit = (type, nullable, unique, def, comment, column) => {
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
              comment,
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
              comment,
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
              currentSchema={currentSchema}
              columnComment={columnComment}
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
              currentSchema={currentSchema}
              columnComment={columnComment}
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
                <Button
                  className={styles.add_mar_small}
                  size="xs"
                  color="white"
                  data-test={`edit-${colName}`}
                  onClick={() => {
                    if (activeEdit.column === colName) {
                      // just closing the column
                      dispatch({ type: TOGGLE_ACTIVE_COLUMN, column: colName });
                    } else {
                      // fetch column comment
                      dispatch(fetchColumnComment(tableName, colName)).then(
                        () => {
                          dispatch({
                            type: TOGGLE_ACTIVE_COLUMN,
                            column: colName,
                          });
                        }
                      );
                    }
                  }}
                >
                  {btnText}
                </Button>
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

    // TODO
    const untrackBtn = (
      <Button
        type="submit"
        className={styles.add_mar_right}
        color="white"
        size="sm"
        onClick={() => {
          const isOk = confirm('Are you sure to untrack?');
          if (isOk) {
            dispatch(untrackTableSql(tableName));
          }
        }}
        data-test="untrack-table"
      >
        Untrack Table
      </Button>
    );

    const editCommentClicked = () => {
      let commentText =
        tableComment && tableComment.result[1] && tableComment.result[1][0]
          ? tableComment.result[1][0]
          : null;
      commentText = commentText !== 'NULL' ? commentText : null;
      dispatch(activateCommentEdit(true, commentText));
    };
    const commentEdited = e => {
      dispatch(updateCommentInput(e.target.value));
    };
    const commentEditSave = () => {
      dispatch(saveTableCommentSql(true));
    };
    const commentEditCancel = () => {
      dispatch(activateCommentEdit(false, ''));
    };
    let commentText =
      tableComment && tableComment.result[1] && tableComment.result[1][0]
        ? tableComment.result[1][0]
        : null;
    commentText = commentText !== 'NULL' ? commentText : null;
    let commentHtml = (
      <div className={styles.add_pad_bottom}>
        <div className={styles.commentText}>Add a comment</div>
        <div onClick={editCommentClicked} className={styles.commentEdit}>
          <i className="fa fa-edit" />
        </div>
      </div>
    );
    if (commentText && !tableCommentEdit.enabled) {
      commentHtml = (
        <div className={styles.mar_bottom}>
          <div className={styles.commentText + ' alert alert-warning'}>
            {commentText}
          </div>
          <div onClick={editCommentClicked} className={styles.commentEdit}>
            <i className="fa fa-edit" />
          </div>
        </div>
      );
    } else if (tableCommentEdit.enabled) {
      commentHtml = (
        <div className={styles.mar_bottom}>
          <input
            onChange={commentEdited}
            className={'form-control ' + styles.commentInput}
            type="text"
            value={tableCommentEdit.value}
            defaultValue={commentText}
          />
          <div
            onClick={commentEditSave}
            className={
              styles.display_inline +
              ' ' +
              styles.add_pad_left +
              ' ' +
              styles.comment_action
            }
          >
            Save
          </div>
          <div
            onClick={commentEditCancel}
            className={
              styles.display_inline +
              ' ' +
              styles.add_pad_left +
              ' ' +
              styles.comment_action
            }
          >
            Cancel
          </div>
        </div>
      );
    }

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
          <div
            className={
              `col-xs-9 ${styles.padd_left_remove}` +
              ' ' +
              styles.modifyMinWidth
            }
          >
            {commentHtml}
            <h4 className={styles.subheading_text}>Columns</h4>
            {columnEditors}
            <hr />
            <h4 className={styles.subheading_text}>Add a new column</h4>
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
                  data-test="column-name"
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
                  data-test="default-value"
                />
                <Button
                  type="submit"
                  color="yellow"
                  size="sm"
                  data-test="add-column-button"
                >
                  + Add column
                </Button>
              </form>
            </div>
            <hr />
            {untrackBtn}
            <Button
              type="submit"
              color="red"
              size="sm"
              onClick={() => {
                const isOk = confirm('Are you sure?');
                if (isOk) {
                  dispatch(deleteTableSql(tableName, tableSchema));
                }
              }}
              data-test="delete-table"
            >
              Delete table
            </Button>
            <br />
            <br />
          </div>
        </div>
      </div>
    );
  }
}

ModifyTable.propTypes = {
  tableName: PropTypes.string.isRequired,
  currentSchema: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  tableComment: PropTypes.string.isRequired,
  columnComment: PropTypes.string.isRequired,
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
  tableComment: state.tables.tableComment,
  columnComment: state.tables.columnComment,
  ...state.tables.modify,
});

const modifyTableConnector = connect => connect(mapStateToProps)(ModifyTable);

export default modifyTableConnector;
