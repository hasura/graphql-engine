/* eslint-disable no-use-before-define*/

import React from 'react';
import PropTypes from 'prop-types';

import {
  addToPrefix,
  getRefTable,
  getTableColumnNames,
  getTableRelationshipNames,
  getTableRelationship,
  boolOperators,
  columnOperators,
  arrayColumnOperators,
} from './utils';

import QueryBuilderJson from '../../../../QueryBuilderJson/QueryBuilderJson';

const styles = require('./Styles.scss');

class PermissionBuilder extends React.Component {
  static propTypes = {
    allSchemas: PropTypes.array.isRequired,
    dispatch: PropTypes.func.isRequired,
    dispatchFunc: PropTypes.func.isRequired,
    filter: PropTypes.string,
    table: PropTypes.string,
  };

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

    const getTableSchemas = allSchemas => {
      const _tableSchemas = {};

      allSchemas.forEach(tableSchema => {
        _tableSchemas[tableSchema.table_name] = {
          columns: tableSchema.columns,
          relationships: tableSchema.relationships,
          foreign_key_constraints: tableSchema.foreign_key_constraints,
        };
      });

      return _tableSchemas;
    };

    const getFilter = (conditions, prefix, value = '') => {
      const _where = {};

      const prefixSplit = prefix.split('.');
      const operation = prefixSplit[0];

      if (prefixSplit.length !== 1) {
        if (operation === boolOperators.or || operation === boolOperators.and) {
          const position = parseInt(prefixSplit[1], 10);
          _where[operation] = conditions[operation];
          _where[operation][position] = getFilter(
            conditions[operation][position],
            prefixSplit.slice(2).join('.'),
            value
          );
          if (Object.keys(_where[operation][position]).length === 0) {
            _where[operation].splice(position, 1);
          }
        } else if (operation === boolOperators.not) {
          _where[operation] = getFilter(
            conditions[operation],
            prefixSplit.slice(1).join('.'),
            value
          );
        } else if (arrayColumnOperators.indexOf(operation) !== -1) {
          const position = parseInt(prefixSplit[1], 10);
          _where[operation] = conditions[operation] || [];
          if (value) {
            _where[operation][position] = value;
          } else {
            _where[operation].splice(position, 1);
          }
          // is column name
        } else {
          _where[operation] = getFilter(
            conditions[operation],
            prefixSplit.slice(1).join('.'),
            value
          );
        }
      } else {
        if (operation === '--') {
          // blank where
        } else if (
          operation === boolOperators.or ||
          operation === boolOperators.and
        ) {
          _where[operation] = [];
        } else if (operation === boolOperators.not) {
          _where[operation] = {};
        } else if (arrayColumnOperators.indexOf(operation) !== -1) {
          _where[operation] = value || [];
        } else if (columnOperators.indexOf(operation) !== -1) {
          _where[operation] = value;
          // if (operation === '$eq') {
          //   _where = value
          // }

          // is column name
        } else {
          _where[operation] = {};
        }
      }

      return _where;
    };

    const _dispatchFunc = data => {
      const { dispatch, filter, dispatchFunc } = this.props;

      const newFilter = getFilter(
        JSON.parse(filter || '{}'),
        data.prefix,
        data.value
      );

      // dispatch(dispatchFunc(JSON.stringify(newFilter, null, 4)));
      dispatch(dispatchFunc(JSON.stringify(newFilter)));
    };

    const renderSelect = (
      dispatchFunc,
      value,
      values,
      prefix,
      customDispatch = false,
      wrapQuotes = true,
      disabledValues = []
    ) => {
      const dispatchSelect = e => {
        if (customDispatch) {
          dispatchFunc(e);
        } else {
          dispatchFunc(e.target.value);
        }
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

      let _select = (
        <select
          value={selectedValue}
          name={value}
          onChange={dispatchSelect}
          className={styles.qb_select}
        >
          {_selectOptions}
        </select>
      );

      if (wrapQuotes) {
        _select = wrapDoubleQuotes(_select);
      }

      return _select;
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
      const dispatchColumnOperator = e => {
        const _prefix = e.target.value;

        dispatchFunc({ prefix: _prefix });
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
        prefix,
        true,
        false
      );

      let valueInput = '';
      if (operator) {
        if (arrayColumnOperators.indexOf(operator) !== -1) {
          valueInput = renderInputArray(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator)
          );
        } else {
          valueInput = renderInput(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator)
          );
        }
      }

      const _operatorExp = [{ key: operatorSelect, value: valueInput }];
      // if (operator === '$eq') {
      //   _operatorExp = valueInput;
      // }

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
        tableSchema = tableSchemas[table];
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
      const dispatchOperation = e => {
        dispatchFunc({ prefix: e.target.value });
      };

      let operation = null;
      if (condition) {
        operation = Object.keys(condition)[0];
      }

      let tableColumns = [];
      let tableRelationships = [];
      if (table) {
        const tableSchema = tableSchemas[table];
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
        true,
        false,
        ['---']
      );

      let boolExpValue = null;
      if (operation) {
        const newPrefix = addToPrefix(prefix, operation);
        if (operation === boolOperators.or || operation === boolOperators.and) {
          boolExpValue = renderBoolExpArray(
            dispatchFunc,
            condition[operation],
            table,
            tableSchemas,
            newPrefix
          );
        } else if (operation === boolOperators.not) {
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
      const { table, filter, allSchemas } = this.props;

      return renderBoolExp(
        _dispatchFunc,
        JSON.parse(filter || '{}'),
        table,
        getTableSchemas(allSchemas)
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
