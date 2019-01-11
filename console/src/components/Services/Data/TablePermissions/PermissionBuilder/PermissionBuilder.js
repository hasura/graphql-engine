/* eslint-disable no-use-before-define*/

import React from 'react';
import PropTypes from 'prop-types';

import {
  addToPrefix,
  getRefTable,
  getTableColumnNames,
  getTableRelationshipNames,
  getTableRelationship,
  getTableDef,
  getTableSchema,
  getAllJsonPaths,
  isNotOperator,
  isAndOrOperator,
  isColumnOperator,
  isBoolColumnOperator,
  isArrayColumnOperator,
  boolOperators,
  columnOperators,
} from './utils';

import QueryBuilderJson from '../../../../QueryBuilderJson/QueryBuilderJson';

const styles = require('./Styles.scss');

class PermissionBuilder extends React.Component {
  static propTypes = {
    allTableSchemas: PropTypes.array.isRequired,
    dispatch: PropTypes.func.isRequired,
    dispatchFuncSetFilter: PropTypes.func.isRequired,
    dispatchFuncAddTableSchemas: PropTypes.func.isRequired,
    filter: PropTypes.string,
    tableName: PropTypes.string,
    schemaName: PropTypes.string,
  };

  componentDidMount() {
    this.fetchMissingSchemas();
  }

  componentDidUpdate(prevProps) {
    if (
      this.props.filter !== prevProps.filter ||
      this.props.allTableSchemas.length !== prevProps.allTableSchemas.length
    ) {
      this.fetchMissingSchemas();
    }
  }

  fetchMissingSchemas() {
    const {
      dispatch,
      tableName,
      schemaName,
      dispatchFuncAddTableSchemas,
      filter,
    } = this.props;

    const findMissingSchemas = (path, currTable) => {
      let _missingSchemas = [];

      const pathSplit = path.split('.');

      const operator = pathSplit[0];

      if (isAndOrOperator(operator)) {
        const newPath = pathSplit.slice(2).join('.');
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isNotOperator(operator)) {
        const newPath = pathSplit.slice(1).join('.');
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (
        isColumnOperator(operator) ||
        isArrayColumnOperator(operator)
      ) {
        // no missing schemas
      } else {
        const { allTableSchemas } = this.props;

        const tableSchema = getTableSchema(allTableSchemas, currTable);
        const tableRelationships = getTableRelationshipNames(tableSchema);

        if (tableRelationships.includes(operator)) {
          const rel = getTableRelationship(tableSchema, operator);
          const refTable = getRefTable(rel, tableSchema);

          const refTableSchema = getTableSchema(allTableSchemas, refTable);
          if (!refTableSchema) {
            _missingSchemas.push(refTable.schema);
          }

          const newPath = pathSplit.slice(1).join('.');
          _missingSchemas.push(...findMissingSchemas(newPath, refTable));
        } else {
          // no missing schemas
        }
      }

      return _missingSchemas;
    };

    const table = getTableDef(tableName, schemaName);

    const missingSchemas = [];
    const paths = getAllJsonPaths(JSON.parse(filter || '{}'));

    for (let i = 0; i < paths.length; i++) {
      const path = paths[i];

      const subMissingSchemas = findMissingSchemas(path, table);

      missingSchemas.push(...subMissingSchemas);
    }

    if (missingSchemas.length > 0) {
      dispatch(dispatchFuncAddTableSchemas(missingSchemas));
    }
  }

  render() {
    const wrapDoubleQuotes = value => {
      return (
        <span>
          "&nbsp;
          {value}
          &nbsp;"
        </span>
      );
    };

    const getFilter = (conditions, prefix, value = '') => {
      let _where = {};

      const getAndOrOperatorFilter = (
        operator,
        opValue,
        opConditions,
        opPrefix,
        isLast
      ) => {
        const _filter = {};

        if (isLast) {
          _filter[operator] = [];
        } else {
          const opPrefixSplit = opPrefix.split('.');

          const position = parseInt(opPrefixSplit[0], 10);
          const newPrefix = opPrefixSplit.slice(1).join('.');

          _filter[operator] = opConditions;
          _filter[operator][position] = getFilter(
            opConditions[position],
            newPrefix,
            opValue
          );
          if (Object.keys(_filter[operator][position]).length === 0) {
            _filter[operator].splice(position, 1);
          }
        }

        return _filter;
      };

      const getNotOperatorFilter = (
        operator,
        opValue,
        opConditions,
        opPrefix,
        isLast
      ) => {
        const _filter = {};

        if (isLast) {
          _filter[operator] = {};
        } else {
          _filter[operator] = getFilter(opConditions, opPrefix, opValue);
        }

        return _filter;
      };

      const getArrayColumnOperatorFilter = (
        operator,
        opValue,
        opConditions,
        opPrefix,
        isLast
      ) => {
        const _filter = {};

        if (isLast) {
          _filter[operator] = opValue || [];
        } else {
          const opPrefixSplit = opPrefix.split('.');
          const position = parseInt(opPrefixSplit[0], 10);

          _filter[operator] = opConditions || [];
          if (opValue) {
            _filter[operator][position] = opValue;
          } else {
            _filter[operator].splice(position, 1);
          }
        }

        return _filter;
      };

      const getColumnOperatorFilter = (operator, opValue) => {
        const _filter = {};

        _filter[operator] = opValue;

        return _filter;
      };

      const getColumnFilter = (
        operator,
        opValue,
        opConditions,
        opPrefix,
        isLast
      ) => {
        const _filter = {};

        if (isLast) {
          _filter[operator] = {};
        } else {
          _filter[operator] = getFilter(opConditions, opPrefix, opValue);
        }

        return _filter;
      };

      const prefixSplit = prefix.split('.');

      const operator = prefixSplit[0];
      const newPrefix = prefixSplit.slice(1).join('.');

      const isLast = prefixSplit.length === 1;

      const opConditions = isLast ? null : conditions[operator];

      if (operator === '--') {
        // blank where
      } else if (isAndOrOperator(operator)) {
        _where = getAndOrOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isNotOperator(operator)) {
        _where = getNotOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isArrayColumnOperator(operator)) {
        _where = getArrayColumnOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isColumnOperator(operator)) {
        _where = getColumnOperatorFilter(operator, value);
      } else {
        _where = getColumnFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      }

      return _where;
    };

    const _dispatchFunc = data => {
      const { dispatch, filter, dispatchFuncSetFilter } = this.props;

      const newFilter = getFilter(
        JSON.parse(filter || '{}'),
        data.prefix,
        data.value
      );

      // dispatch(dispatchFuncSetFilter(JSON.stringify(newFilter, null, 4)));
      dispatch(dispatchFuncSetFilter(JSON.stringify(newFilter)));
    };

    const renderBoolSelect = (dispatchFunc, value) => {
      const boolDispatchFunc = val => {
        let boolVal = '';
        if (val === 'true') {
          boolVal = true;
        } else if (val === 'false') {
          boolVal = false;
        }

        dispatchFunc(boolVal);
      };

      let selectValue = '';
      if (value === true) {
        selectValue = 'true';
      } else if (value === false) {
        selectValue = 'false';
      }

      const selectValues = ['true', 'false'];

      return renderSelect(boolDispatchFunc, selectValue, selectValues);
    };

    const renderSelect = (
      dispatchFunc,
      value,
      values,
      prefix = '',
      disabledValues = []
    ) => {
      const dispatchSelect = e => {
        dispatchFunc(e.target.value);
      };

      const _selectOptions = [];
      ['--'].concat(values).forEach((val, i) => {
        const optionVal = addToPrefix(prefix, val);
        _selectOptions.push(
          <option
            value={optionVal}
            key={i}
            disabled={disabledValues.indexOf(val) !== -1}
          >
            {val}
          </option>
        );
      });

      const selectedValue = addToPrefix(prefix, value || '--');

      return (
        <select
          value={selectedValue}
          name={value}
          onChange={dispatchSelect}
          className={styles.qb_select}
          data-test="qb-select"
        >
          {_selectOptions}
        </select>
      );
    };

    const renderInput = (dispatchFunc, value, prefix) => {
      const dispatchInput = e => {
        dispatchFunc({ prefix: prefix, value: e.target.value });
      };

      const dispatchSuggestion = () => {
        dispatchFunc({ prefix: prefix, value: 'X-HASURA-USER-ID' });
      };

      const input = wrapDoubleQuotes(
        <input
          value={value}
          onChange={dispatchInput}
          type="text"
          className={styles.qb_input}
          data-test="perm-check-textbox"
        />
      );

      const suggestion = (
        <span
          onClick={dispatchSuggestion}
          className={styles.qb_input_suggestion}
        >
          [X-Hasura-User-Id]
        </span>
      );

      return (
        <span>
          {input} {suggestion}
        </span>
      );
    };

    const renderInputArray = (dispatchFunc, values, prefix) => {
      const _inputArray = [];
      (values || []).concat(['']).map((val, i) => {
        const input = renderInput(dispatchFunc, val, addToPrefix(prefix, i));
        _inputArray.push(input);
      });

      const unselectedElements = [(values || []).length];

      return (
        <QueryBuilderJson
          element={_inputArray}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderOperatorExp = (dispatchFunc, condition, prefix) => {
      const dispatchColumnOperator = val => {
        dispatchFunc({ prefix: val });
      };

      let _condition = condition;
      if (typeof _condition === 'string') {
        _condition = { $eq: _condition };
      }

      const operator = Object.keys(_condition)[0];
      const operationValue = _condition[operator];

      const operatorSelect = renderSelect(
        dispatchColumnOperator,
        operator,
        columnOperators,
        prefix
      );

      let valueInput = '';
      if (operator) {
        if (isArrayColumnOperator(operator)) {
          valueInput = renderInputArray(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator)
          );
        } else if (isBoolColumnOperator(operator)) {
          const boolOperatorDispatchFunc = val => {
            dispatchFunc({ prefix: addToPrefix(prefix, operator), value: val });
          };

          valueInput = renderBoolSelect(
            boolOperatorDispatchFunc,
            operationValue
          );
        } else {
          // normal column operator
          valueInput = renderInput(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator)
          );
        }
      }

      const _operatorExp = [{ key: operatorSelect, value: valueInput }];

      const unselectedElements = [];
      if (!operator) {
        unselectedElements.push(0);
      }

      return (
        <QueryBuilderJson
          element={_operatorExp}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderColumnExp = (
      dispatchFunc,
      column,
      condition,
      table,
      tableSchemas,
      prefix
    ) => {
      let tableRelationships = [];
      let tableSchema;
      if (table) {
        tableSchema = getTableSchema(tableSchemas, table);
        tableRelationships = getTableRelationshipNames(tableSchema);
      }

      let _columnExp = '';
      if (tableRelationships.indexOf(column) !== -1) {
        const rel = getTableRelationship(tableSchema, column);
        const refTable = getRefTable(rel, tableSchema);

        _columnExp = renderBoolExp(
          dispatchFunc,
          condition,
          refTable,
          tableSchemas,
          prefix
        ); // eslint-disable-line no-use-before-define
      } else {
        _columnExp = renderOperatorExp(dispatchFunc, condition, prefix);
      }

      return _columnExp;
    };

    const renderBoolExpArray = (
      dispatchFunc,
      conditions,
      table,
      tableSchemas,
      prefix
    ) => {
      const _boolExpArray = [];

      conditions.concat([{}]).forEach((condition, i) => {
        const _boolExp = renderBoolExp(
          dispatchFunc,
          condition,
          table,
          tableSchemas,
          addToPrefix(prefix, i)
        ); // eslint-disable-line no-use-before-define
        _boolExpArray.push(_boolExp);
      });

      const unselectedElements = [conditions.length];

      return (
        <QueryBuilderJson
          element={_boolExpArray}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderBoolExp = (
      dispatchFunc,
      condition,
      table,
      tableSchemas,
      prefix = ''
    ) => {
      const dispatchOperation = val => {
        dispatchFunc({ prefix: val });
      };

      let operation = null;
      if (condition) {
        operation = Object.keys(condition)[0];
      }

      let tableColumns = [];
      let tableRelationships = [];
      if (table) {
        const tableSchema = getTableSchema(tableSchemas, table);
        tableColumns = getTableColumnNames(tableSchema);
        tableRelationships = getTableRelationshipNames(tableSchema);
      }

      const columnOptions = tableColumns.concat(tableRelationships);
      const operationOptions = Object.values(boolOperators);

      const operatorOptions = columnOptions
        .concat(['---'])
        .concat(operationOptions);

      const boolExpKey = renderSelect(
        dispatchOperation,
        operation,
        operatorOptions,
        prefix,
        ['---']
      );

      let boolExpValue = null;
      if (operation) {
        const newPrefix = addToPrefix(prefix, operation);
        if (isAndOrOperator(operation)) {
          boolExpValue = renderBoolExpArray(
            dispatchFunc,
            condition[operation],
            table,
            tableSchemas,
            newPrefix
          );
        } else if (isNotOperator(operation)) {
          boolExpValue = renderBoolExp(
            dispatchFunc,
            condition[operation],
            table,
            tableSchemas,
            newPrefix
          );
        } else {
          boolExpValue = renderColumnExp(
            dispatchFunc,
            operation,
            condition[operation],
            table,
            tableSchemas,
            newPrefix
          );
        }
      }

      const _boolExp = [{ key: boolExpKey, value: boolExpValue }];

      const unselectedElements = [];
      if (!operation) {
        unselectedElements.push(0);
      }

      return (
        <QueryBuilderJson
          element={_boolExp}
          unselectedElements={unselectedElements}
        />
      );
    };

    const showPermissionBuilder = () => {
      const { tableName, schemaName, filter, allTableSchemas } = this.props;

      const table = getTableDef(tableName, schemaName);

      return renderBoolExp(
        _dispatchFunc,
        JSON.parse(filter || '{}'),
        table,
        allTableSchemas
      );
    };

    return (
      <div className="container-fluid">
        <div className="row">
          <div className={styles.qb_container}>
            <div className={styles.remove_margin_bottom + ' well'}>
              {showPermissionBuilder()}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default PermissionBuilder;
