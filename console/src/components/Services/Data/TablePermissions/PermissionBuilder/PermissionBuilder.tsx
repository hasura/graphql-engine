/* eslint-disable @typescript-eslint/no-use-before-define */
import React from 'react';

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
  deprecatedColumnOperators,
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
import SelectGroup, { OptGroup, QuotedSelectGroup } from './SelectGroup';
import {
  getComputedFieldFunction,
  getGroupedTableComputedFields,
  getComputedFieldsWithoutArgs,
} from '../../../../../dataSources/services/postgresql';
import { QualifiedTable } from '../../../../../metadata/types';
import styles from './PermissionBuilder.module.scss';
import { ComputedField, Table } from '../../../../../dataSources/types';
import { Nullable } from '../../../../Common/utils/tsUtils';
import { PGFunction } from '../../../../../dataSources/services/postgresql/types';

interface PermissionBuilderProps {
  allTableSchemas: Table[];
  allFunctions: PGFunction[];
  schemaList: string[];
  dispatch: () => void;
  dispatchFuncSetFilter: (filter: string) => void;
  loadSchemasFunc: (schemaNames: string[]) => void;
  filter: string;
  tableDef: QualifiedTable;
}

class PermissionBuilder extends React.Component<PermissionBuilderProps> {
  override componentDidMount() {
    this.loadMissingSchemas();
  }

  override componentDidUpdate(prevProps: PermissionBuilderProps) {
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

    const findMissingSchemas = (
      path: Record<string, any> | string[] | string,
      currTable: QualifiedTable
    ): string[] => {
      let missingSchemas = [];

      let value: any;
      if (isObject(path)) {
        value = Object.values(path)[0];
        path = Object.keys(path)[0];
      }

      const getNewPath = (newPath: string) => {
        return value ? { [newPath]: value } : newPath;
      };

      const pathSplit = (path as string).split('.');

      const operator = pathSplit[0];

      if (isArrayBoolOperator(operator)) {
        const newPath = getNewPath(pathSplit.slice(2).join('.'));
        missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isBoolOperator(operator)) {
        const newPath = getNewPath(pathSplit.slice(1).join('.'));
        missingSchemas = findMissingSchemas(newPath, currTable);
      } else if (isExistOperator(operator)) {
        const existTableDef = getQualifiedTableDef(value[TABLE_KEY]);

        let existTableSchema;
        if (existTableDef) {
          existTableSchema = (existTableDef as QualifiedTable).schema;
        }

        const existWhere = value[WHERE_KEY] || '';

        if (existTableSchema) {
          const { allTableSchemas } = this.props;

          const allSchemaNames = allTableSchemas.map(t => t.table_schema);

          if (!allSchemaNames.includes(existTableSchema)) {
            missingSchemas.push(existTableSchema);
          }
        }

        this.loadMissingSchemas(
          existTableDef as QualifiedTable,
          JSON.stringify(existWhere)
        );
      } else if (isColumnOperator(operator)) {
        // no missing schemas
      } else {
        const { allTableSchemas } = this.props;

        let tableRelationshipNames: string[] = [];

        const tableSchema = findTable(allTableSchemas, currTable);

        if (tableSchema) {
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
        }

        if (tableRelationshipNames.includes(operator)) {
          const relationship = getTableRelationship(tableSchema!, operator);
          const refTable = getRelationshipRefTable(tableSchema!, relationship!);

          const refTableSchema = findTable(allTableSchemas, refTable!);
          if (!refTableSchema) {
            missingSchemas.push(refTable!.schema);
          }

          const newPath = getNewPath(pathSplit.slice(1).join('.'));
          missingSchemas.push(...findMissingSchemas(newPath, refTable!));
        } else {
          // no missing schemas
        }
      }

      return missingSchemas;
    };

    const missingSchemas: string[] = [];
    const paths = getAllJsonPaths(JSON.parse(filter || '{}'), existOperators);

    paths.forEach((path: Record<string, any> | string) => {
      const subMissingSchemas = findMissingSchemas(path, tableDef);
      missingSchemas.push(...subMissingSchemas);
    });

    if (missingSchemas.length > 0) {
      loadSchemasFunc(missingSchemas);
    }
  }

  override render() {
    const wrapDoubleQuotes = (value: JSX.Element) => {
      return (
        <span>
          &quot;&nbsp;
          {value}
          &nbsp;&quot;
        </span>
      );
    };

    /* ****************************** */

    const getFilter = (
      defaultSchema: string,
      conditions: Record<string, any>,
      prefix: string,
      value = ''
    ) => {
      let boolExp = {};

      const getArrayBoolOperatorFilter = (
        operator: string,
        opValue: string,
        opConditions: Record<string, any>[],
        opPrefix: string,
        isLast: boolean
      ) => {
        const filter: Record<string, Record<string, any>> = {};

        if (isLast) {
          filter[operator] = [];
        } else {
          const opPrefixSplit = opPrefix.split('.');

          const position = parseInt(opPrefixSplit[0], 10);
          const newPrefix = opPrefixSplit.slice(1).join('.');

          filter[operator] = opConditions;
          filter[operator][position] = getFilter(
            defaultSchema,
            opConditions[position],
            newPrefix,
            opValue
          );
          if (Object.keys(filter[operator][position]).length === 0) {
            filter[operator].splice(position, 1);
          }
        }

        return filter;
      };

      const getBoolOperatorFilter = (
        operator: string,
        opValue: string,
        opConditions: Record<string, any>[],
        opPrefix: string,
        isLast: boolean
      ) => {
        const filter: Record<string, Record<string, any>> = {};

        if (isLast) {
          filter[operator] = {};
        } else {
          filter[operator] = getFilter(
            defaultSchema,
            opConditions,
            opPrefix,
            opValue
          );
        }

        return filter;
      };

      const getArrayColumnOperatorFilter = (
        operator: string,
        opValue: string,
        opConditions: string[],
        opPrefix: string,
        isLast: boolean
      ) => {
        const filter: Record<string, any> = {};

        if (isLast) {
          filter[operator] = opValue || [];
        } else {
          const opPrefixSplit = opPrefix.split('.');
          const position = parseInt(opPrefixSplit[0], 10);

          filter[operator] = opConditions || [];
          if (opValue !== '') {
            filter[operator][position] = opValue;
          } else {
            filter[operator].splice(position, 1);
          }
        }

        return filter;
      };

      const getColumnOperatorFilter = (operator: string, opValue: string) => {
        const filter: Record<string, string> = {};
        filter[operator] = opValue;
        return filter;
      };

      const getExistsOperatorFilter = (
        operator: string,
        // HACK: justifitied any
        opValue: any,
        opConditions: Record<string, Record<string, any>>,
        opPrefix: string,
        isLast: boolean
      ) => {
        const filter = {
          [operator]: opConditions,
        };

        if (isLast) {
          filter[operator] = {
            [TABLE_KEY]: generateTableDef('', defaultSchema),
            [WHERE_KEY]: {},
          };
        } else if (opPrefix === TABLE_KEY) {
          filter[operator] = {
            [TABLE_KEY]: opValue,
            [WHERE_KEY]: {},
          };
        } else if (opPrefix === WHERE_KEY) {
          filter[operator][WHERE_KEY] = getFilter(
            defaultSchema,
            opConditions[opPrefix],
            opValue.prefix,
            opValue.value
          );
        }

        return filter;
      };

      const getColumnFilter = (
        operator: string,
        opValue: string,
        opConditions: Record<string, any>,
        opPrefix: string,
        isLast: boolean
      ) => {
        const filter: Record<string, Record<string, any>> = {};

        if (isLast) {
          filter[operator] = {};
        } else {
          filter[operator] = getFilter(
            defaultSchema,
            opConditions,
            opPrefix,
            opValue
          );
        }

        return filter;
      };

      const prefixSplit = prefix.split('.');

      const operator = prefixSplit[0];
      const newPrefix = prefixSplit.slice(1).join('.');

      const isLast = prefixSplit.length === 1;

      const opConditions = isLast ? null : conditions[operator];

      if (operator === '') {
        // blank bool exp
      } else if (isArrayBoolOperator(operator)) {
        boolExp = getArrayBoolOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isBoolOperator(operator)) {
        boolExp = getBoolOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isArrayColumnOperator(operator)) {
        boolExp = getArrayColumnOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else if (isColumnOperator(operator)) {
        boolExp = getColumnOperatorFilter(operator, value);
      } else if (isExistOperator(operator)) {
        boolExp = getExistsOperatorFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      } else {
        boolExp = getColumnFilter(
          operator,
          value,
          opConditions,
          newPrefix,
          isLast
        );
      }

      return boolExp;
    };

    // eslint-disable-next-line no-underscore-dangle
    const _dispatchFunc = (data: { prefix: string; value?: string }) => {
      const { filter, dispatchFuncSetFilter, tableDef } = this.props;
      const newFilter = getFilter(
        tableDef.schema,
        JSON.parse(filter || '{}'),
        data.prefix,
        data.value
      );
      dispatchFuncSetFilter(JSON.stringify(newFilter));
    };

    /* ****************************** */

    const renderSelect = (
      selectDispatchFunc: (val: string) => void,
      value: string,
      values: string[] | OptGroup[],
      prefix = '',
      disabledValues: string[] = []
    ) => {
      const dispatchSelect = (e: React.ChangeEvent<HTMLSelectElement>) => {
        selectDispatchFunc(e.target.value);
      };

      if (typeof values?.[1] !== 'string') {
        return (
          <SelectGroup
            selectDispatchFunc={selectDispatchFunc}
            value={value}
            values={values as OptGroup[]}
            prefix={prefix}
            disabledValues={disabledValues}
          />
        );
      }

      const selectOptions: JSX.Element[] = [];
      [''].concat(values as string[]).forEach((val, i) => {
        const optionVal = addToPrefix(prefix, val);
        selectOptions.push(
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
          {selectOptions}
        </select>
      );
    };

    const renderBoolSelect = (
      selectDispatchFunc: (val: string) => void,
      value: string,
      prefix = '',
      disabledValues = []
    ) => {
      const newValue = typeof value === 'boolean' ? `${value}` : '';

      const values = ['true', 'false'];

      return renderSelect(
        selectDispatchFunc,
        newValue,
        values,
        prefix,
        disabledValues
      );
    };

    const renderInput = (
      inputDispatchFunc: (val: string) => void,
      value: string
    ) => {
      const dispatchInput = (e: React.ChangeEvent<HTMLInputElement>) => {
        inputDispatchFunc(e.target.value);
      };

      let inputValue = value;

      if (typeof value === 'object') {
        inputValue = JSON.stringify(value);
      }

      return (
        <input
          value={inputValue}
          onChange={dispatchInput}
          type="text"
          className={styles.qb_input}
          data-test="perm-check-textbox"
        />
      );
    };

    const renderSuggestion = (
      suggestionDispatchFunc: (val: string) => void,
      inputValue: string,
      displayValue: string | null = null
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

    /* ****************************** */

    const renderValue = (
      dispatchFunc: ({
        prefix,
        value,
      }: {
        prefix: string;
        value: string | number | boolean;
      }) => void,
      value: string,
      prefix: string,
      valueType: string,
      tableColumns: string[] | OptGroup[],
      showSuggestion = true
    ): JSX.Element => {
      const currentTypeMap = dataSource.permissionColumnDataTypes;
      if (!currentTypeMap) {
        // shouldn't happen ideally. check in place for the MySQL `null`
        return <></>;
      }
      const dispatchInput = (val: string | number) => {
        let newValue: typeof val | boolean = val;

        if (val !== '') {
          if (
            currentTypeMap?.boolean &&
            currentTypeMap.boolean.includes(valueType)
          ) {
            newValue = val === 'true';
          } else if (
            currentTypeMap?.numeric &&
            currentTypeMap.numeric.includes(valueType) &&
            !isNaN(val as number) &&
            (val as string).substr(-1) !== '.'
          ) {
            newValue = Number(val);
          } else if (
            currentTypeMap?.jsonb &&
            currentTypeMap.jsonb.includes(valueType) &&
            isJsonString(val as string)
          ) {
            newValue = JSON.parse(val as string);
          }
        }
        dispatchFunc({ prefix, value: newValue });
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
        (currentDriver === 'alloy' ||
          currentDriver === 'postgres' ||
          currentDriver === 'bigquery')
      ) {
        input = renderBoolSelect(dispatchInput, value);
      } else if (
        (currentTypeMap?.jsonb &&
          currentTypeMap.jsonb.includes(valueType) &&
          currentDriver === 'postgres') ||
        currentDriver === 'alloy'
      ) {
        input = inputBox();
        suggestion = jsonSuggestion();
      } else if (valueType === 'column') {
        if (typeof tableColumns?.[0] === 'string') {
          input = wrapDoubleQuotes(
            renderSelect(
              dispatchInput,
              value as string,
              tableColumns as string[]
            )
          );
        } else if (tableColumns?.[0]?.optGroupTitle) {
          input = (
            <QuotedSelectGroup
              selectDispatchFunc={dispatchInput}
              value={value as string}
              values={tableColumns as OptGroup[]}
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

    const renderColumnArray = (
      dispatchFunc: (arg: {
        prefix: string;
        value: Array<string | number | boolean>;
      }) => void,
      values: string[],
      prefix: string,
      valueType: string
    ) => {
      const { tableDef, allTableSchemas } = this.props;
      const rootTable = findTable(allTableSchemas, tableDef)!;
      let prevTable: Table | null = getTableFromRelationshipChain(
        allTableSchemas,
        rootTable,
        prefix
      );

      const inputArray = (values?.length < 1 ? [''] : [])
        .concat(values || [])
        .concat([''])
        .map((val, i, arr) => {
          const onChange = (v: {
            prefix: string;
            value: string | number | boolean;
          }) => {
            dispatchFunc({
              prefix: v.prefix,
              value: [...arr.slice(0, i), v.value],
            });
          };

          const options: OptGroup[] = [];
          // uncomment options.relationships assignment to enable selection of relationships
          if (i === 0) {
            options.push({ optGroupTitle: 'root', options: ['$'] });
            // options.push({optGroupTitle: 'relationships', options: getTableRelationshipNames(prevTable)});
            options.push({
              optGroupTitle: 'columns',
              options: getTableColumnNames(prevTable!),
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
                const def = getRelationshipRefTable(prevTable, rel)!;
                prevTable = findTable(allTableSchemas, def)!;
                if (prevTable) {
                  // options.push({optGroupTitle: 'relationships', options: getTableRelationshipNames(prevTable)});
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
      // TODO: fix any
      dispatchFunc: (arg: { prefix: string; value: any }) => void,
      values: string[],
      prefix: string,
      valueType: string,
      tableColumns: string[]
    ) => {
      if (valueType === 'column') {
        return renderColumnArray(dispatchFunc, values, prefix, valueType);
      }
      const dispatchInput = (val: any) => {
        dispatchFunc({ prefix, value: val });
      };

      const sessionVariableSuggestion = () => {
        return renderSuggestion(dispatchInput, 'X-Hasura-Allowed-Ids');
      };

      const inputArray: JSX.Element[] = [];

      (values || []).concat(['']).forEach((val: string, i: number) => {
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

      const inputArrayJSX = (
        <QueryBuilderJson
          element={inputArray}
          unselectedElements={unselectedElements}
        />
      );

      const suggestionJSX = sessionVariableSuggestion();

      return (
        <span>
          {inputArrayJSX} {suggestionJSX}
        </span>
      );
    };

    const renderOperatorExp = (
      dispatchFunc: (arg0: { prefix: string }) => void,
      // HACK: impossible to type
      expression: any,
      prefix: string,
      valueType: string,
      tableColumns: string[]
    ) => {
      const dispatchColumnOperatorSelect = (val: string) => {
        dispatchFunc({ prefix: val });
      };

      // handle shorthand notation for eq
      let newExpression = expression;
      if (typeof newExpression !== 'object') {
        newExpression = { _eq: newExpression };
      }

      const operator = Object.keys(newExpression)[0];
      const operationValue = newExpression[operator];

      const currentTypeMap = dataSource.permissionColumnDataTypes;
      const rootValueType = getRootType(valueType, currentTypeMap);
      const operators = (
        getPermissionOperators(
          dataSource.supportedColumnOperators,
          currentTypeMap
        ) as Record<string, string[]>
      )[rootValueType];

      const operatorSelect = renderSelect(
        dispatchColumnOperatorSelect,
        operator,
        deprecatedColumnOperators.includes(operator)
          ? operators.concat(operator)
          : operators,
        prefix
      );

      let valueInput = null;
      if (operator) {
        const operatorInputType = getOperatorInputType(operator) || valueType;

        if (
          isArrayColumnOperator(operator) &&
          operationValue instanceof Array
        ) {
          valueInput = renderValueArray(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator),
            operatorInputType,
            tableColumns
          );
        } else {
          valueInput = renderValue(
            dispatchFunc,
            operationValue,
            addToPrefix(prefix, operator),
            operatorInputType,
            tableColumns
          );
        }
      }

      const operatorExp = [{ key: operatorSelect, value: valueInput }];

      const unselectedElements = [];
      if (!operator) {
        unselectedElements.push(0);
      }

      return (
        <QueryBuilderJson
          element={operatorExp}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderColumnExp = (
      dispatchFunc: (data: { prefix: string; value?: string }) => void,
      columnName: string,
      expression: Record<string, string>,
      tableDef: QualifiedTable,
      tableSchemas: Table[],
      schemaList: string[],
      prefix: string
    ) => {
      let tableColumnNames: string[] = [];
      let tableRelationshipNames: string[] = [];
      let computedFieldFn;
      let tableSchema: Table | null = null;
      if (tableDef) {
        tableSchema = findTable(tableSchemas, tableDef)!;
        if (tableSchema) {
          tableColumnNames = getTableColumnNames(tableSchema);
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
          const { allFunctions } = this.props;
          const computedFields = getGroupedTableComputedFields(
            tableSchema.computed_fields,
            allFunctions
          );
          const computedField = computedFields.scalar.find(
            cs => cs.computed_field_name === columnName
          );
          if (computedField) {
            computedFieldFn = getComputedFieldFunction(
              computedField,
              allFunctions
            );
          }
        }
      }

      let columnExp = null;
      if (tableRelationshipNames.includes(columnName)) {
        const relationship = getTableRelationship(tableSchema!, columnName);
        const refTable = getRelationshipRefTable(tableSchema!, relationship!);

        columnExp = renderBoolExp(
          dispatchFunc,
          expression,
          refTable!,
          tableSchemas,
          schemaList,
          prefix
        );
      } else if (computedFieldFn) {
        columnExp = renderOperatorExp(
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

        columnExp = renderOperatorExp(
          dispatchFunc,
          expression,
          prefix,
          columnType,
          tableColumnNames
        );
      }

      return columnExp;
    };

    const renderTableSelect = (
      dispatchFunc: (tableDef: QualifiedTable) => void,
      tableDef: QualifiedTable,
      tableSchemas: Table[],
      schemaList: string[] | OptGroup[],
      defaultSchema: string
    ) => {
      const selectedSchema = tableDef ? tableDef.schema : defaultSchema;
      const selectedTable = tableDef ? tableDef.name : '';

      const schemaSelectDispatchFunc = (val: Nullable<string>) => {
        dispatchFunc(generateTableDef('', val));
      };

      const tableSelectDispatchFunc = (val: string) => {
        dispatchFunc(generateTableDef(val, selectedSchema));
      };

      const tableNames = getSchemaTableNames(tableSchemas, selectedSchema);

      const schemaSelect = wrapDoubleQuotes(
        renderSelect(schemaSelectDispatchFunc, selectedSchema, schemaList)
      );

      const tableSelect = wrapDoubleQuotes(
        renderSelect(tableSelectDispatchFunc, selectedTable, tableNames)
      );

      const tableExp = [
        { key: 'schema', value: schemaSelect },
        { key: 'name', value: tableSelect },
      ];

      return <QueryBuilderJson element={tableExp} />;
    };

    const renderExistsExp = (
      dispatchFunc: (arg0: { prefix: string; value: any }) => void,
      operation: string,
      expression: Record<string, any>,
      tableDef: QualifiedTable,
      tableSchemas: Table[],
      schemaList: string[],
      prefix: string
    ) => {
      const dispatchTableSelect = (val: QualifiedTable) => {
        dispatchFunc({ prefix: addToPrefix(prefix, TABLE_KEY), value: val });
      };

      const dispatchWhereOperatorSelect = (val: {
        prefix: string;
        value?: string;
      }) => {
        dispatchFunc({ prefix: addToPrefix(prefix, WHERE_KEY), value: val });
      };

      const existsOpTable = getQualifiedTableDef(
        expression[TABLE_KEY]
      ) as QualifiedTable;
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

      const existsArgsJsonObject = {
        [TABLE_KEY]: tableSelect,
        [WHERE_KEY]: whereSelect,
      };

      const unselectedElements = [];
      if (!existsOpTable || !existsOpTable.name) {
        unselectedElements.push(WHERE_KEY);
      }

      return (
        <QueryBuilderJson
          element={existsArgsJsonObject}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderBoolExpArray = (
      dispatchFunc: (data: {
        prefix: string;
        value?: string | undefined;
      }) => void,
      expressions: any[],
      tableDef: QualifiedTable,
      tableSchemas: Table[],
      schemaList: string[],
      prefix: string
    ) => {
      const boolExpArray: JSX.Element[] = [];
      expressions = isArray(expressions) ? expressions : [];

      expressions.concat([{}]).forEach((expression: any, i: number) => {
        const boolExp = renderBoolExp(
          dispatchFunc,
          expression,
          tableDef,
          tableSchemas,
          schemaList,
          addToPrefix(prefix, i)
        );
        boolExpArray.push(boolExp);
      });

      const unselectedElements = [expressions.length];

      return (
        <QueryBuilderJson
          element={boolExpArray}
          unselectedElements={unselectedElements}
        />
      );
    };

    const renderBoolExp = (
      dispatchFunc: (data: { prefix: string; value?: string }) => void,
      expression: Record<string, any>,
      tableDef: QualifiedTable,
      tableSchemas: Table[],
      schemaList: string[],
      prefix = ''
    ) => {
      const dispatchOperationSelect = (val: string) => {
        dispatchFunc({ prefix: val });
      };

      let operation = '';
      if (expression) {
        operation = Object.keys(expression)[0];
      }

      let tableColumnNames: string[] = [];
      let tableRelationshipNames: string[] = [];
      let scalarComputedFieldsWithoutArgs: ComputedField[] = [];
      if (tableDef) {
        const tableSchema = findTable(tableSchemas, tableDef);
        if (tableSchema) {
          tableColumnNames = getTableColumnNames(tableSchema);
          tableRelationshipNames = getTableRelationshipNames(tableSchema);
          const { allFunctions } = this.props;
          const computedFields = getGroupedTableComputedFields(
            tableSchema.computed_fields,
            allFunctions
          );
          scalarComputedFieldsWithoutArgs = getComputedFieldsWithoutArgs(
            computedFields.scalar,
            allFunctions,
            tableDef.name
          );
        }
      }

      const computedFieldsOptions = scalarComputedFieldsWithoutArgs.map(
        f => f.computed_field_name
      );
      const newOperatorOptions = [
        { optGroupTitle: 'bool operators', options: boolOperators },
        { optGroupTitle: 'exist operators', options: existOperators },
        { optGroupTitle: 'columns', options: tableColumnNames },
        { optGroupTitle: 'relationships', options: tableRelationshipNames },
        { optGroupTitle: 'computed fields', options: computedFieldsOptions },
      ];

      const boolExpKey = renderSelect(
        dispatchOperationSelect,
        operation,
        newOperatorOptions,
        prefix,
        ['---']
      );

      let boolExpValue = null;
      if (operation) {
        const newPrefix = addToPrefix(prefix, operation);
        if (isArrayBoolOperator(operation)) {
          boolExpValue = renderBoolExpArray(
            dispatchFunc,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else if (isBoolOperator(operation)) {
          boolExpValue = renderBoolExp(
            dispatchFunc,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else if (isExistOperator(operation)) {
          boolExpValue = renderExistsExp(
            dispatchFunc,
            operation,
            expression[operation],
            tableDef,
            tableSchemas,
            schemaList,
            newPrefix
          );
        } else {
          boolExpValue = renderColumnExp(
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

      const boolExp = [{ key: boolExpKey, value: boolExpValue }];

      const unselectedElements = [];
      if (!operation) {
        unselectedElements.push(0);
      }

      return (
        <QueryBuilderJson
          element={boolExp}
          unselectedElements={unselectedElements}
        />
      );
    };

    /* ****************************** */

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
            <div className={`${styles.remove_margin_bottom} well`}>
              {showPermissionBuilder()}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default PermissionBuilder;
