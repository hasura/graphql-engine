import { ArrayRelationInsertFieldValue, ColumnInsertFieldValue, DeleteMutationOperation, Expression, Field, InsertFieldSchema, InsertMutationOperation, MutationOperation, MutationOperationResults, MutationRequest, MutationResponse, ObjectRelationInsertFieldValue, Relationships, RowUpdate, TableInsertSchema, TableName, TableRelationships, UpdateMutationOperation } from "@hasura/dc-api-types";
import { Config } from "./config";
import { Connection, defaultMode, SqlLogger, withConnection } from "./db";
import { escapeIdentifier, escapeTableName, escapeTableNameSansSchema, json_object, where_clause, } from "./query";
import { asyncSequenceFromInputs, ErrorWithStatusCode, mapObjectToArray, tableNameEquals, tableToTarget, unreachable } from "./util";

// Types

type Row = {
  ok: boolean,
  row: string,
  statement?: string,
  values?: Record<string, unknown>,
}

type RowInfoValue = ColumnInsertFieldValue | ObjectRelationInsertFieldValue | ArrayRelationInsertFieldValue | null;

type RowInfo = {
  field: string,
  schema: InsertFieldSchema,
  variable: string,
  value: RowInfoValue
}

type UpdateInfo = {
  variable: string,
  value: unknown,
  update: RowUpdate,
}

interface Info {
  variable: string,
  value: unknown
}

// Functions

function escapeVariable(x: string): string {
  return x.replace(/[^a-zA-Z0-9]/g, '');
}

function valuesString(infos: Array<RowInfo>): string {
  return infos.map((info) => info.variable).join(', ');
}

function customUpdateOperator(operator: string, column: string, variable: string): string {
  switch(operator) {
    case 'inc':
      return `${column} + ${variable}`;
    case 'dec':
      return `${column} - ${variable}`;
  }
  throw Error(`Custom operator ${operator} is invalid. This should not happen.`);
}

function setString(infos: Array<UpdateInfo>): string {
  return infos.map((info) => {
    const update = info.update;
    switch(update.type) {
      case 'custom_operator':
        return `${update.column} = ${customUpdateOperator(update.operator_name, update.column, info.variable)}`
      case 'set':
        return `${update.column} = ${info.variable}`
      default:
        return unreachable(update);
    }
  }).join(', ');
}

function columnsString(infos: Array<RowInfo>): string {
  return infos.map((info) => {
    switch(info.schema.type) {
      case 'column':
        return escapeIdentifier(info.schema.column);
      default:
        throw(Error(`Type ${info.schema.type} for field ${info.field} is not currently supported.`))
    }
  }).join(', ');
}

/**
 * @param schemas
 * @param table
 * @returns schema | null
 */
function getTableInsertSchema(schemas: Array<TableInsertSchema>, table: TableName): TableInsertSchema | null {
  for(var i = 0; i < schemas.length; i++) {
    const schema = schemas[i];
    if(tableNameEquals(schema.table)(tableToTarget(table))) {
      return schema;
    }
  }
  return null;
}

/**
 *
 * @param e
 * @returns boolean check on returned data
 *
 * Note: The heavy lifting is performed by `where_clause` from query.ts
 */
function whereString(relationships: Array<Relationships>, e: Expression, table: TableName): string {
  const w = where_clause(relationships, e, tableToTarget(table), escapeTableNameSansSchema(table));
  return w;
}

/**
 * @param op
 * @returns SQLite expression that can be used in RETURNING clauses
 *
 * The `json_object` function from query.ts performs the heavy lifting here.
 */
function returningString(relationships: Array<Relationships>, fields: Record<string, Field>, table: TableName): string {
  /* Example of fields:
    {
      "ArtistId": {
        "type": "column",
        "column": "ArtistId",
        "column_type": "number"
      }
    }
  */
  const r = json_object(relationships, fields, tableToTarget(table), escapeTableNameSansSchema(table));
  return r;
}

function queryValues(info: Array<Info>): Record<string, unknown> {
  return Object.fromEntries(info.map((x) => [x.variable, x.value]));
}

const EMPTY_AND: Expression = { type: 'and', expressions: [] };

function insertString(relationships: Array<Relationships>, op: InsertMutationOperation, info: Array<RowInfo>): string {
  const columnValues =
    info.length > 0
      ? `(${columnsString(info)}) VALUES (${valuesString(info)})`
      : "DEFAULT VALUES";

  return `
    INSERT INTO ${escapeTableName(op.table)} ${columnValues}
    RETURNING
      ${returningString(relationships, op.returning_fields || {}, op.table)} as row,
      ${whereString(relationships, op.post_insert_check || EMPTY_AND, op.table)} as ok
  `;
}

function deleteString(relationships: Array<Relationships>, op: DeleteMutationOperation): string {
  return `
    DELETE FROM ${escapeTableName(op.table)}
    WHERE ${whereString(relationships, op.where || EMPTY_AND, op.table)}
    RETURNING
      ${returningString(relationships, op.returning_fields || {}, op.table)} as row,
      1=1 as ok
  `;
}

function updateString(relationships: Array<Relationships>, op: UpdateMutationOperation, info: Array<UpdateInfo>): string {
  const result = `
    UPDATE ${escapeTableName(op.table)}
    SET ${setString(info)}
    WHERE ${whereString(relationships, op.where || EMPTY_AND, op.table)}
    RETURNING
      ${returningString(relationships, op.returning_fields || {}, op.table)} as row,
      ${whereString(relationships, op.post_update_check || EMPTY_AND, op.table)} as ok
  `;
  return result;
}

/**
 * @param schemas
 * @param op
 * @returns Nested Array of RowInfo
 *
 * This function compiles all the useful information for constructing query-strings and variable data
 * into arrays of RowInfo packets. It is done this way to avoid repeated lookups and to keep the alignment
 * of identifiers, variables, and data in sync.
 */
function getInsertRowInfos(schemas: Array<TableInsertSchema>, op: InsertMutationOperation): Array<RowInfo[]> {
  const schema = getTableInsertSchema(schemas, op.table);
  if(schema == null) {
    throw(Error(`Couldn't find insert schema for table ${escapeTableName(op.table)}`));
  }
  return op.rows.map((row, rowIndex) => {
    const rowInfo = mapObjectToArray(row, ([fieldName,fieldValue], fieldIndex) => {
      const fieldSchema = schema.fields[fieldName];
      if(fieldSchema == null) {
        throw(Error(`Couldn't find insert schema for field ${fieldName} for table ${escapeTableName(op.table)}`));
      }
      return {
        field: fieldName,
        schema: fieldSchema,
        variable: `$${escapeVariable(fieldName)}_${rowIndex}_${fieldIndex}`,
        value: fieldValue
      };
    });
    return rowInfo;
  });
}

function getUpdateRowInfos(op: UpdateMutationOperation): Array<UpdateInfo> {
  return op.updates.map((update, i) => {
    return {
      variable: `$${escapeVariable(update.column)}_${i}`,
      value: update.value,
      update: update
    };
  });
}

async function insertRow(db: Connection, relationships: Array<Relationships>, op: InsertMutationOperation, info: Array<RowInfo>):  Promise<Array<Row>> {
  const q = insertString(relationships, op, info);
  const v = queryValues(info);
  const results = await db.query(q,v);
  results.forEach((r) => {
    if(!r.ok) {
      r.statement = q
      r.values = v
    }
  });
  return results;
}

async function updateRow(db: Connection, relationships: Array<Relationships>, op: UpdateMutationOperation, info: Array<UpdateInfo>):  Promise<Array<Row>> {
  const q = updateString(relationships, op, info);
  const v = queryValues(info);
  const results = await db.query(q,v);
  results.forEach((r) => {
    if(!r.ok) {
      r.statement = q
      r.values = v
    }
  });
  return results;
}

async function deleteRows(db: Connection, relationships: Array<Relationships>, op: DeleteMutationOperation):  Promise<Array<Row>> {
  const q = deleteString(relationships, op);
  const results = await db.query(q);
  return results;
}

function postMutationCheckError(op: MutationOperation, failed: Array<Row>): ErrorWithStatusCode {
  return ErrorWithStatusCode.mutationPermissionCheckFailure(
    "check constraint of an insert/update permission has failed",
    {op: op, results: failed}
  );
}

async function mutationOperation(db: Connection, relationships: Array<Relationships>, schema: Array<TableInsertSchema>, op: MutationOperation): Promise<MutationOperationResults> {
  switch(op.type) {
    case 'insert':
      const infos = getInsertRowInfos(schema, op);
      await db.query('BEGIN',{});
      // We split this operation into multiple inserts in case the inserted columns are hetrogenous: https://sqlite.org/forum/info/d7384e085b808b05
      const insertResultsSet = await asyncSequenceFromInputs(infos, (info) => insertRow(db, relationships, op, info));
      const insertResults = ([] as Array<Row>).concat(...insertResultsSet);
      let insertFailed: Array<Row> = [];
      const mappedInsertResults = insertResults.map((row: Row) => {
        if (!row.ok) {
          insertFailed.push(row);
        }
        return JSON.parse(row.row);
      });
      if(insertFailed.length > 0) {
        await db.query('ROLLBACK', {});
        throw(postMutationCheckError(op, insertFailed));
      } else {
        await db.query('COMMIT', {});
        return {
          affected_rows: mappedInsertResults.length,
          ...(op.returning_fields ? { returning: mappedInsertResults } : {})
        };
      }

    case 'update':
      const updateInfo = getUpdateRowInfos(op);
      await db.query('BEGIN',{});
      const resultSet = await updateRow(db, relationships, op, updateInfo);
      const updateResults = ([] as Array<Row>).concat(...resultSet);
      let updateFailed: Array<Row> = [];
      const mappedUpdateResults = updateResults.map((row: Row) => {
        if (!row.ok) {
          updateFailed.push(row);
        }
        return JSON.parse(row.row);
      });
      if(updateFailed.length > 0) {
        await db.query('ROLLBACK', {});
        throw(postMutationCheckError(op, updateFailed));
      } else {
        await db.query('COMMIT', {});
        return {
          affected_rows: mappedUpdateResults.length,
          ...(op.returning_fields ? { returning: mappedUpdateResults } : {})
        };
      }

    case 'delete':
      await db.query('BEGIN',{});
      const deleteResults = await deleteRows(db, relationships, op);
      const mappedDeleteResults = deleteResults.map(row => JSON.parse(row.row));
      await db.query('COMMIT',{});
      return {
        affected_rows: mappedDeleteResults.length,
        ...(op.returning_fields ? { returning: mappedDeleteResults } : {})
      };

    default:
      return unreachable(op['type']);
  }
}

/**
 * @param config
 * @param sqlLogger
 * @param request
 * @returns MutationResponse
 *
 * Top-Level function for mutations.
 * This performs inserts/updates/deletes.
 */
export async function runMutation(config: Config, sqlLogger: SqlLogger, request: MutationRequest): Promise<MutationResponse> {
  return await withConnection(config, defaultMode, sqlLogger, async db => {
    const resultSet = await asyncSequenceFromInputs(request.operations, (op) => mutationOperation(db, request.relationships, request.insert_schema, op));
    return {
      operation_results: resultSet
    };
  });
}
