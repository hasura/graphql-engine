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
  getColumnType,
  isJsonString,
  getAllJsonPaths,
  isArrayBoolOperator,
  isBoolOperator,
  isArrayColumnOperator,
  isColumnOperator,
  getRootPGType,
  getOperatorInputType,
  boolOperators,
  PGTypes,
  PGTypesOperators,
} from './utils';

import QueryBuilderJson from '../../../../Common/QueryBuilderJson/QueryBuilderJson';

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
    // check for and fetch any missing schemas if
    // either permission filter or available table schemas have changed
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

      if (isArrayBoolOperator(operator)) {
        const newPath = pathSplit.slice(2).join('.');
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isBoolOperator(operator)) {
        const newPath = pathSplit.slice(1).join('.');
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isColumnOperator(operator)) {
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
    const styles = require('./PermissionBuilder.scss');

    const wrapDoubleQuotes = value => {
      return (
        <span>
          "&nbsp;
          {value}
          &nbsp;"
        </span>
      );
    };

    /********************************/

    const getFilter = (conditions, prefix, value = '') => {
      let _where = {};

      const getArrayBoolOperatorFilter = (
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

      const getBoolOperatorFilter = (
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
          if (opValue !== '') {
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

      if (operator === '') {
        // blank where
      } else if (isArrayBoolOperator(operator)) {
        _where = getArrayBoolOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isBoolOperator(operator)) {
        _where = getBoolOperatorFilter(
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

    /********************************/

    const renderBoolSelect = (
      selectDispatchFunc,
      value,
      prefix = '',
      disabledValues = []
    ) => {
      const _value = typeof value === 'boolean' ? value.toString() : '';

      const values = ['true', 'false'];

      return renderSelect(
        selectDispatchFunc,
        _value,
        values,
        prefix,
        disabledValues
      );
    };

    const renderSelect = (
      selectDispatchFunc,
      value,
      values,
      prefix = '',
      disabledValues = []
    ) => {
      const dispatchSelect = e => {
        selectDispatchFunc(e.target.value);
      };

      const _selectOptions = [];
      [''].concat(values).forEach((val, i) => {
        const optionVal = addToPrefix(prefix, val);
        _selectOptions.push(
          <option
            value={optionVal}
            key={i}
            disabled={disabledValues.includes(val)}
          >
            {val || '--'}
          </option>
        );
      });

      const selectedValue = addToPrefix(prefix, value);

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

    const renderInput = (inputDispatchFunc, value) => {
      const dispatchInput = e => {
        inputDispatchFunc(e.target.value);
      };

      let _value = value;

      if (typeof value === 'object') {
        _value = JSON.stringify(value);
      }

      return (
        <input
          value={_value}
          onChange={dispatchInput}
          type="text"
          className={styles.qb_input}
          data-test="perm-check-textbox"
        />
      );
    };

    const renderSuggestion = (
      suggestionDispatchFunc,
      inputValue,
      displayValue = null
    ) => {
      const dispatchSuggestion = () => {
        suggestionDispatchFunc(inputValue);
      };

      return (
        <span
          onClick={dispatchSuggestion}
          className={styles.qb_input_suggestion}
        >
          [{displayValue || inputValue}]
        </span>
      );
    };

    /********************************/

    const renderValue = (
      dispatchFunc,
      value,
      prefix,
      valueType,
      showSuggestion = true
    ) => {
      const dispatchInput = val => {
        let _val = val;

        if (val !== '') {
          if (PGTypes.boolean.includes(valueType)) {
            _val = val === 'true';
          } else if (
            PGTypes.numeric.includes(valueType) &&
            !isNaN(val) &&
            val.substr(-1) !== '.'
          ) {
            _val = Number(val);
          } else if (PGTypes.json.includes(valueType) && isJsonString(val)) {
            _val = JSON.parse(val);
          }
        }

        dispatchFunc({ prefix: prefix, value: _val });
      };

      const inputBox = () => {
        return renderInput(dispatchInput, value);
      };

      const sessionVariableSuggestion = () => {
        return renderSuggestion(dispatchInput, 'X-Hasura-User-Id');
      };

      const jsonSuggestion = () => {
        return renderSuggestion(dispatchInput, '{}', 'JSON');
      };

      let input;
      let suggestion;

      if (PGTypes.boolean.includes(valueType)) {
        input = renderBoolSelect(dispatchInput, value);
      } else if (PGTypes.json.includes(valueType)) {
        input = inputBox();
        suggestion = jsonSuggestion();
      } else {
        input = wrapDoubleQuotes(inputBox());
        suggestion = sessionVariableSuggestion();
      }

      return (
        <span>
          {input} {showSuggestion ? suggestion : ''}
        </span>
      );
    };

    const renderValueArray = (dispatchFunc, values, prefix, valueType) => {
      const dispatchInput = val => {
        dispatchFunc({ prefix: prefix, value: val });
      };

      const sessionVariableSuggestion = () => {
        return renderSuggestion(dispatchInput, 'X-Hasura-Allowed-Ids');
      };

      const inputArray = [];

      (values || []).concat(['']).map((val, i) => {
        const input = renderValue(
          dispatchFunc,
          val,
          addToPrefix(prefix, i),
          valueType,
          false
        );
        inputArray.push(input);
      });

      const unselectedElements = [(values || []).length];

      const _inputArray = (
        <QueryBuilderJson
          element={inputArray}
          unselectedElements={unselectedElements}
        />
      );

      const _suggestion = sessionVariableSuggestion(dispatchInput);

      return (
        <span>
          {_inputArray} {_suggestion}
        </span>
      );
    };

    const renderOperatorExp = (dispatchFunc, expression, prefix, valueType) => {
      const dispatchColumnOperatorSelect = val => {
        dispatchFunc({ prefix: val });
      };

      // handle shorthand notation for eq
      let _expression = expression;
      if (typeof _expression !== 'object') {
        _expression = { _eq: _expression };
      }

      const operator = Object.keys(_expression)[0];
      const operationValue = _expression[operator];

      const rootValueType = getRootPGType(valueType);
      const operators = PGTypesOperators[rootValueType];

      const _operatorSelect = renderSelect(
        dispatchColumnOperatorSelect,
        operator,
        operators,
        prefix
      );

      let _valueInput = '';
      if (operator) {
        const operatorInputType = getOperatorInputType(operator) || valueType;

        if (
          isArrayColumnOperator(operator) &&
          operationValue instanceof Array
        ) {
          _valueInput = renderValueArray(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator),
            operatorInputType
          );
        } else {
          _valueInput = renderValue(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator),
            operatorInputType
          );
        }
      }

      const _operatorExp = [{ key: _operatorSelect, value: _valueInput }];

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
      expression,
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
      if (tableRelationships.includes(column)) {
        const rel = getTableRelationship(tableSchema, column);
        const refTable = getRefTable(rel, tableSchema);

        _columnExp = renderBoolExp(
          dispatchFunc,
          expression,
          refTable,
          tableSchemas,
          prefix
        ); // eslint-disable-line no-use-before-define
      } else {
        const columnType = getColumnType(column, tableSchema);

        _columnExp = renderOperatorExp(
          dispatchFunc,
          expression,
          prefix,
          columnType
        );
      }

      return _columnExp;
    };

    const renderBoolExpArray = (
      dispatchFunc,
      expressions,
      table,
      tableSchemas,
      prefix
    ) => {
      const _boolExpArray = [];

      expressions.concat([{}]).forEach((expression, i) => {
        const _boolExp = renderBoolExp(
          dispatchFunc,
          expression,
          table,
          tableSchemas,
          addToPrefix(prefix, i)
        ); // eslint-disable-line no-use-before-define
        _boolExpArray.push(_boolExp);
      });

      const unselectedElements = [expressions.length];

      return (
        <QueryBuilderJson
          element={_boolExpArray}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderBoolExp = (
      dispatchFunc,
      expression,
      table,
      tableSchemas,
      prefix = ''
    ) => {
      const dispatchOperationSelect = val => {
        dispatchFunc({ prefix: val });
      };

      let operation = null;
      if (expression) {
        operation = Object.keys(expression)[0];
      }

      let tableColumns = [];
      let tableRelationships = [];
      if (table) {
        const tableSchema = getTableSchema(tableSchemas, table);
        tableColumns = getTableColumnNames(tableSchema);
        tableRelationships = getTableRelationshipNames(tableSchema);
      }

      const columnOptions = tableColumns.concat(tableRelationships);

      const operatorOptions = boolOperators
        .concat(['---'])
        .concat(columnOptions);

      const _boolExpKey = renderSelect(
        dispatchOperationSelect,
        operation,
        operatorOptions,
        prefix,
        ['---']
      );

      let _boolExpValue = null;
      if (operation) {
        const newPrefix = addToPrefix(prefix, operation);
        if (isArrayBoolOperator(operation)) {
          _boolExpValue = renderBoolExpArray(
            dispatchFunc,
            expression[operation],
            table,
            tableSchemas,
            newPrefix
          );
        } else if (isBoolOperator(operation)) {
          _boolExpValue = renderBoolExp(
            dispatchFunc,
            expression[operation],
            table,
            tableSchemas,
            newPrefix
          );
        } else {
          _boolExpValue = renderColumnExp(
            dispatchFunc,
            operation,
            expression[operation],
            table,
            tableSchemas,
            newPrefix
          );
        }
      }

      const _boolExp = [{ key: _boolExpKey, value: _boolExpValue }];

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

    /********************************/

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
