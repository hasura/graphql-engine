/* eslint-disable no-use-before-define*/

import React from 'react';

import PropTypes from 'prop-types';

import {
  currentDriver,
  dataSource,
  findTable,
  generateTableDef,
  getQualifiedTableDef,
  getRelationshipRefTable,
  getSchemaTableNames,
  getTableColumn,
  getTableColumnNames,
  getTableRelationship,
  getTableRelationshipNames,
  getTrackedTables,
  getTableFromRelationshipChain,
} from '../../../../../dataSources';
import QueryBuilderJson from '../../../../Common/QueryBuilderJson/QueryBuilderJson';
import {
  getAllJsonPaths,
  isArray,
  isJsonString,
  isObject,
} from '../../../../Common/utils/jsUtils';
import {
  addToPrefix,
  boolOperators,
  existOperators,
  getOperatorInputType,
  getPermissionOperators,
  getRootType,
  isArrayBoolOperator,
  isArrayColumnOperator,
  isBoolOperator,
  isColumnOperator,
  isExistOperator,
  TABLE_KEY,
  WHERE_KEY,
} from './utils';
import SelectGroup, { QuotedSelectGroup } from './SelectGroup';
import {
  getComputedFieldFunction,
  getGroupedTableComputedFields,
} from '../../../../../dataSources/services/postgresql';

class PermissionBuilder extends React.Component {
  static propTypes = {
    allTableSchemas: PropTypes.array.isRequired,
    allFunctions: PropTypes.array.isRequired,
    schemaList: PropTypes.array.isRequired,
    dispatch: PropTypes.func.isRequired,
    dispatchFuncSetFilter: PropTypes.func.isRequired,
    loadSchemasFunc: PropTypes.func.isRequired,
    filter: PropTypes.string,
    tableDef: PropTypes.object.isRequired,
  };

  componentDidMount() {
    this.loadMissingSchemas();
  }

  componentDidUpdate(prevProps) {
    // check for and fetch any missing schemas if
    // either permission filter or available table schemas have changed
    if (
      this.props.filter !== prevProps.filter ||
      this.props.allTableSchemas.length !== prevProps.allTableSchemas.length
    ) {
      this.loadMissingSchemas();
    }
  }

  loadMissingSchemas(
    tableDef = this.props.tableDef,
    filter = this.props.filter
  ) {
    const { loadSchemasFunc } = this.props;

    const findMissingSchemas = (path, currTable) => {
      let _missingSchemas = [];

      let value;
      if (isObject(path)) {
        value = Object.values(path)[0];
        path = Object.keys(path)[0];
      }

      const getNewPath = newPath => {
        return value ? { [newPath]: value } : newPath;
      };

      const pathSplit = path.split('.');

      const operator = pathSplit[0];

      if (isArrayBoolOperator(operator)) {
        const newPath = getNewPath(pathSplit.slice(2).join('.'));
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isBoolOperator(operator)) {
        const newPath = getNewPath(pathSplit.slice(1).join('.'));
        _missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isExistOperator(operator)) {
        const existTableDef = getQualifiedTableDef(value[TABLE_KEY]);

        let existTableSchema;
        if (existTableDef) {
          existTableSchema = existTableDef.schema;
        }

        const existWhere = value[WHERE_KEY] || '';

        if (existTableSchema) {
          const { allTableSchemas } = this.props;

          const allSchemaNames = allTableSchemas.map(t => t.table_schema);

          if (!allSchemaNames.includes(existTableSchema)) {
            _missingSchemas.push(existTableSchema);
          }
        }

        this.loadMissingSchemas(existTableDef, JSON.stringify(existWhere));
      } else if (isColumnOperator(operator)) {
        // no missing schemas
      } else {
        const { allTableSchemas } = this.props;

        let tableRelationshipNames = [];

        const tableSchema = findTable(allTableSchemas, currTable);

        if (tableSchema) {
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
        }

        if (tableRelationshipNames.includes(operator)) {
          const relationship = getTableRelationship(tableSchema, operator);
          const refTable = getRelationshipRefTable(tableSchema, relationship);

          const refTableSchema = findTable(allTableSchemas, refTable);
          if (!refTableSchema) {
            _missingSchemas.push(refTable.schema);
          }

          const newPath = getNewPath(pathSplit.slice(1).join('.'));
          _missingSchemas.push(...findMissingSchemas(newPath, refTable));
        } else {
          // no missing schemas
        }
      }

      return _missingSchemas;
    };

    const missingSchemas = [];
    const paths = getAllJsonPaths(JSON.parse(filter || '{}'), existOperators);

    paths.forEach(path => {
      const subMissingSchemas = findMissingSchemas(path, tableDef);

      missingSchemas.push(...subMissingSchemas);
    });

    if (missingSchemas.length > 0) {
      loadSchemasFunc(missingSchemas);
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

    const getFilter = (defaultSchema, conditions, prefix, value = '') => {
      let _boolExp = {};

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
            defaultSchema,
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
          _filter[operator] = getFilter(
            defaultSchema,
            opConditions,
            opPrefix,
            opValue
          );
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

      const getExistsOperatorFilter = (
        operator,
        opValue,
        opConditions,
        opPrefix,
        isLast
      ) => {
        const _filter = {
          [operator]: opConditions,
        };

        if (isLast) {
          _filter[operator] = {
            [TABLE_KEY]: generateTableDef('', defaultSchema),
            [WHERE_KEY]: {},
          };
        } else if (opPrefix === TABLE_KEY) {
          _filter[operator] = {
            [TABLE_KEY]: opValue,
            [WHERE_KEY]: {},
          };
        } else if (opPrefix === WHERE_KEY) {
          _filter[operator][WHERE_KEY] = getFilter(
            defaultSchema,
            opConditions[opPrefix],
            opValue.prefix,
            opValue.value
          );
        }

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
          _filter[operator] = getFilter(
            defaultSchema,
            opConditions,
            opPrefix,
            opValue
          );
        }

        return _filter;
      };

      const prefixSplit = prefix.split('.');

      const operator = prefixSplit[0];
      const newPrefix = prefixSplit.slice(1).join('.');

      const isLast = prefixSplit.length === 1;

      const opConditions = isLast ? null : conditions[operator];

      if (operator === '') {
        // blank bool exp
      } else if (isArrayBoolOperator(operator)) {
        _boolExp = getArrayBoolOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isBoolOperator(operator)) {
        _boolExp = getBoolOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isArrayColumnOperator(operator)) {
        _boolExp = getArrayColumnOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isColumnOperator(operator)) {
        _boolExp = getColumnOperatorFilter(operator, value);
      } else if (isExistOperator(operator)) {
        _boolExp = getExistsOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else {
        _boolExp = getColumnFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      }

      return _boolExp;
    };

    const _dispatchFunc = data => {
      const { filter, dispatchFuncSetFilter, tableDef } = this.props;

      const newFilter = getFilter(
        tableDef.schema,
        JSON.parse(filter || '{}'),
        data.prefix,
        data.value
      );

      // dispatchFuncSetFilter(JSON.stringify(newFilter, null, 4));
      dispatchFuncSetFilter(JSON.stringify(newFilter));
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

      if (typeof values?.[1] !== 'string') {
        return (
          <SelectGroup
            selectDispatchFunc={selectDispatchFunc}
            value={value}
            values={values}
            prefix={prefix}
            disabledValues={disabledValues}
          />
        );
      }

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

      const selectedValue = addToPrefix(prefix, value || '--');

      return (
        <select
          value={selectedValue}
          name={prefix}
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
      tableColumns,
      showSuggestion = true
    ) => {
      const currentTypeMap = dataSource.permissionColumnDataTypes;
      if (!currentTypeMap) {
        // shouldn't happen ideally. check in place for the MySQL `null`
        return;
      }
      const dispatchInput = val => {
        let _val = val;

        if (val !== '') {
          if (
            currentTypeMap?.boolean &&
            currentTypeMap.boolean.includes(valueType)
          ) {
            _val = val === 'true';
          } else if (
            currentTypeMap?.numeric &&
            currentTypeMap.numeric.includes(valueType) &&
            !isNaN(val) &&
            val.substr(-1) !== '.'
          ) {
            _val = Number(val);
          } else if (
            currentTypeMap?.jsonb &&
            currentTypeMap.jsonb.includes(valueType) &&
            isJsonString(val)
          ) {
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

      if (
        currentTypeMap?.boolean &&
        currentTypeMap.boolean.includes(valueType) &&
        currentDriver === 'postgres'
      ) {
        input = renderBoolSelect(dispatchInput, value);
      } else if (
        currentTypeMap?.jsonb &&
        currentTypeMap.jsonb.includes(valueType) &&
        currentDriver === 'postgres'
      ) {
        input = inputBox();
        suggestion = jsonSuggestion();
      } else if (valueType === 'column') {
        if (typeof tableColumns?.[0] === 'string') {
          input = wrapDoubleQuotes(
            renderSelect(dispatchInput, value, tableColumns)
          );
        } else if (tableColumns?.[0]?.optGroupTitle) {
          input = (
            <QuotedSelectGroup
              selectDispatchFunc={dispatchInput}
              value={value}
              values={tableColumns}
            />
          );
        }
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

    const renderColumnArray = (dispatchFunc, values, prefix, valueType) => {
      const { tableDef, allTableSchemas } = this.props;
      const rootTable = findTable(allTableSchemas, tableDef);
      let prevTable = getTableFromRelationshipChain(
        allTableSchemas,
        rootTable,
        prefix
      );

      const inputArray = (values?.length < 1 ? [''] : [])
        .concat(values || [])
        .concat([''])
        .map((val, i, arr) => {
          const onChange = v => {
            dispatchFunc({
              prefix: v.prefix,
              value: [...arr.slice(0, i), v.value],
            });
          };

          const options = [];
          // uncomment options.relationships assignment to enable selection of relationships
          if (i === 0) {
            options.push({ optGroupTitle: 'root', options: ['$'] });
            // options.push({optGroupTitle: 'relationships', options: getTableRelationshipNames(prevTable)});
            options.push({
              optGroupTitle: 'columns',
              options: getTableColumnNames(prevTable),
            });
          } else if (arr[i - 1] === '$') {
            // options.push({optGroupTitle: 'relationships', options: getTableRelationshipNames(rootTable)});
            options.push({
              optGroupTitle: 'columns',
              options: getTableColumnNames(rootTable),
            });
            prevTable = rootTable;
          } else if (arr[i - 1]?.length) {
            if (prevTable) {
              const rel = getTableRelationship(prevTable, arr[i - 1]);
              if (rel) {
                const def = getRelationshipRefTable(prevTable, rel);
                prevTable = findTable(allTableSchemas, def);
                if (prevTable) {
                  // options.push({optGroupTitle: 'relationships', options: getTableRelationshipNames(prevTable)});
                  options.columns = getTableColumnNames(prevTable);
                  options.push({
                    optGroupTitle: 'columns',
                    options: getTableColumnNames(prevTable),
                  });
                } else {
                  return null;
                }
              } else {
                prevTable = null;
              }
            }
          }
          return renderValue(onChange, val, prefix, valueType, options, false);
        });

      const unselectedElements = [(values || []).length];

      return (
        <span>
          <QueryBuilderJson
            element={inputArray}
            unselectedElements={unselectedElements}
          />
        </span>
      );
    };

    const renderValueArray = (
      dispatchFunc,
      values,
      prefix,
      valueType,
      tableColumns
    ) => {
      if (valueType === 'column') {
        return renderColumnArray(dispatchFunc, values, prefix, valueType);
      }
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
          tableColumns,
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

    const renderOperatorExp = (
      dispatchFunc,
      expression,
      prefix,
      valueType,
      tableColumns
    ) => {
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

      const currentTypeMap = dataSource.permissionColumnDataTypes;
      const rootValueType = getRootType(valueType, currentTypeMap);
      const operators = getPermissionOperators(
        dataSource.supportedColumnOperators,
        currentTypeMap
      )[rootValueType];

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
            operatorInputType,
            tableColumns
          );
        } else {
          _valueInput = renderValue(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator),
            operatorInputType,
            tableColumns
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
      columnName,
      expression,
      tableDef,
      tableSchemas,
      schemaList,
      prefix
    ) => {
      let tableColumnNames = [];
      let tableRelationshipNames = [];
      let computedFieldFn;
      let tableSchema;
      if (tableDef) {
        tableSchema = findTable(tableSchemas, tableDef);
        if (tableSchema) {
          tableColumnNames = getTableColumnNames(tableSchema);
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
          const { allFunctions } = this.props;
          const computedFields = getGroupedTableComputedFields(
            tableSchema,
            allFunctions
          );
          const computedField = computedFields.scalar.find(
            cs => cs.name === columnName
          );
          if (computedField) {
            computedFieldFn = getComputedFieldFunction(
              computedField,
              allFunctions
            );
          }
        }
      }

      let _columnExp = '';
      if (tableRelationshipNames.includes(columnName)) {
        const relationship = getTableRelationship(tableSchema, columnName);
        const refTable = getRelationshipRefTable(tableSchema, relationship);

        _columnExp = renderBoolExp(
          dispatchFunc,
          expression,
          refTable,
          tableSchemas,
          schemaList,
          prefix
        );
      } else if (computedFieldFn) {
        _columnExp = renderOperatorExp(
          dispatchFunc,
          expression,
          prefix,
          computedFieldFn?.return_type_name,
          tableColumnNames
        );
      } else {
        let columnType = '';
        if (tableSchema && columnName) {
          const column = getTableColumn(tableSchema, columnName);
          if (column) {
            columnType = dataSource.getColumnType(column);
          }
        }

        _columnExp = renderOperatorExp(
          dispatchFunc,
          expression,
          prefix,
          columnType,
          tableColumnNames
        );
      }

      return _columnExp;
    };

    const renderTableSelect = (
      dispatchFunc,
      tableDef,
      tableSchemas,
      schemaList,
      defaultSchema
    ) => {
      const selectedSchema = tableDef ? tableDef.schema : defaultSchema;
      const selectedTable = tableDef ? tableDef.name : '';

      const schemaSelectDispatchFunc = val => {
        dispatchFunc(generateTableDef('', val));
      };

      const tableSelectDispatchFunc = val => {
        dispatchFunc(generateTableDef(val, selectedSchema));
      };

      const tableNames = getSchemaTableNames(tableSchemas, selectedSchema);

      const schemaSelect = wrapDoubleQuotes(
        renderSelect(schemaSelectDispatchFunc, selectedSchema, schemaList)
      );

      const tableSelect = wrapDoubleQuotes(
        renderSelect(tableSelectDispatchFunc, selectedTable, tableNames)
      );

      const _tableExp = [
        { key: 'schema', value: schemaSelect },
        { key: 'name', value: tableSelect },
      ];

      return <QueryBuilderJson element={_tableExp} />;
    };

    const renderExistsExp = (
      dispatchFunc,
      operation,
      expression,
      tableDef,
      tableSchemas,
      schemaList,
      prefix
    ) => {
      const dispatchTableSelect = val => {
        dispatchFunc({ prefix: addToPrefix(prefix, TABLE_KEY), value: val });
      };

      const dispatchWhereOperatorSelect = val => {
        dispatchFunc({ prefix: addToPrefix(prefix, WHERE_KEY), value: val });
      };

      const existsOpTable = getQualifiedTableDef(expression[TABLE_KEY]);
      const existsOpWhere = expression[WHERE_KEY];

      const tableSelect = renderTableSelect(
        dispatchTableSelect,
        existsOpTable,
        tableSchemas,
        schemaList,
        tableDef.schema
      );

      let whereSelect = {};
      if (existsOpTable) {
        whereSelect = renderBoolExp(
          dispatchWhereOperatorSelect,
          existsOpWhere,
          existsOpTable,
          tableSchemas,
          schemaList
        );
      }

      const _existsArgsJsonObject = {
        [TABLE_KEY]: tableSelect,
        [WHERE_KEY]: whereSelect,
      };

      const unselectedElements = [];
      if (!existsOpTable || !existsOpTable.name) {
        unselectedElements.push(WHERE_KEY);
      }

      return (
        <QueryBuilderJson
          element={_existsArgsJsonObject}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderBoolExpArray = (
      dispatchFunc,
      expressions,
      tableDef,
      tableSchemas,
      schemaList,
      prefix
    ) => {
      const _boolExpArray = [];
      expressions = isArray(expressions) ? expressions : [];

      expressions.concat([{}]).forEach((expression, i) => {
        const _boolExp = renderBoolExp(
          dispatchFunc,
          expression,
          tableDef,
          tableSchemas,
          schemaList,
          addToPrefix(prefix, i)
        );
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
      tableDef,
      tableSchemas,
      schemaList,
      prefix = ''
    ) => {
      const dispatchOperationSelect = val => {
        dispatchFunc({ prefix: val });
      };

      let operation = null;
      if (expression) {
        operation = Object.keys(expression)[0];
      }

      let tableColumnNames = [];
      let tableRelationshipNames = [];
      let scalarComputedFields = [];
      if (tableDef) {
        const tableSchema = findTable(tableSchemas, tableDef);
        if (tableSchema) {
          tableColumnNames = getTableColumnNames(tableSchema);
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
          const { allFunctions } = this.props;
          const computedFields = getGroupedTableComputedFields(
            tableSchema,
            allFunctions
          );
          scalarComputedFields = computedFields.scalar.filter(sc => {
            const cFn = getComputedFieldFunction(sc, allFunctions)
              ?.input_arg_types;
            // Only the computed fields that do not require extra arguments other than the table row
            // are currenlty supported by the server https://github.com/hasura/graphql-engine/issues/7336
            return cFn?.length === 1 && cFn[0].name === tableDef.name;
          });
        }
      }

      const computedFieldsOptions = scalarComputedFields.map(f => f.name);
      const newOperatorOptions = [
        { optGroupTitle: 'bool operators', options: boolOperators },
        { optGroupTitle: 'exist operators', options: existOperators },
        { optGroupTitle: 'columns', options: tableColumnNames },
        { optGroupTitle: 'relationships', options: tableRelationshipNames },
        { optGroupTitle: 'computed fields', options: computedFieldsOptions },
      ];

      const _boolExpKey = renderSelect(
        dispatchOperationSelect,
        operation,
        newOperatorOptions,
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
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else if (isBoolOperator(operation)) {
          _boolExpValue = renderBoolExp(
            dispatchFunc,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else if (isExistOperator(operation)) {
          _boolExpValue = renderExistsExp(
            dispatchFunc,
            operation,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else {
          _boolExpValue = renderColumnExp(
            dispatchFunc,
            operation,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
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
      const { tableDef, filter, allTableSchemas, schemaList } = this.props;

      const trackedTables = getTrackedTables(allTableSchemas);

      return renderBoolExp(
        _dispatchFunc,
        JSON.parse(filter || '{}'),
        tableDef,
        trackedTables,
        schemaList
      );
    };

    return (
      <div className="container-fluid">
        <div className="row">
          <div className={styles.qb_container} data-test="qb_container">
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
