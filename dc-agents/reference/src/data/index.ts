import { SchemaRequest, SchemaResponse, TableInfo, TableName } from "@hasura/dc-api-types"
import { Casing, Config } from "../config";
import xml2js from "xml2js"
import fs from "fs"
import stream from "stream"
import zlib from "zlib"
import { parseNumbers } from "xml2js/lib/processors";
import { mapObject, mapObjectValues, nameEquals, unreachable } from "../util";
import { defaultDbStoreName, getDbStoreName } from "../datasets";

export type StaticData = {
  schema: SchemaResponse,
  data: {
    [tableName: string]: Record<string, string | number | boolean | null>[]
  }
}

const streamToBuffer = async (stream: stream.Readable): Promise<Buffer> => {
  const chunks = [];
  for await (const chunk of stream) {
    chunks.push(Buffer.from(chunk));
  }
  return Buffer.concat(chunks);
}

// Only parse numeric columns as numbers, otherwise you get "number-like" columns like BillingPostCode
// getting partially parsed as a number or a string depending on the individual postcode
const parseNumbersInNumericColumns = (schema: SchemaResponse) => {
  const numericColumns = new Set(schema.tables.flatMap(table => (table.columns ?? []).filter(c => c.type === "number").map(c => c.name)));

  return (value: string, name: string): any => {
    return numericColumns.has(name)
      ? parseNumbers(value)
      : value;
  };
}

export const staticDataExists = async(name: string): Promise<boolean> => {
  const dataOK = await new Promise((resolve) => {
    const dataPath = mkDataPath(name);
    fs.access(dataPath, fs.constants.R_OK, err => err ? resolve(false) : resolve(true));
  });
  const schemaOK = await new Promise((resolve) => {
    const schemaPath = mkSchemaPath(name);
    fs.access(schemaPath, fs.constants.R_OK, err => err ? resolve(false) : resolve(true));
  });
  return dataOK as boolean && schemaOK as boolean;
}

const mkDataPath = (name: string): string => {
  return `${__dirname}/${name}.xml.gz`;
}

const mkSchemaPath = (name: string): string => {
  return `${__dirname}/${name}.schema.json`;
}

export const loadStaticData = async (name: string): Promise<StaticData> => {
  // Schema
  const schemaPath = `${__dirname}/${name}.schema.json`;
  const schema: SchemaResponse = JSON.parse(fs.readFileSync(schemaPath, 'utf-8'));

  // Data
  const dataPath = mkDataPath(name);
  const gzipReadStream = fs.createReadStream(dataPath);
  const unzipStream = stream.pipeline(gzipReadStream, zlib.createGunzip(), () => { });
  const xmlStr = (await streamToBuffer(unzipStream)).toString("utf16le");
  const xml = await xml2js.parseStringPromise(xmlStr, { explicitArray: false, emptyTag: () => null, valueProcessors: [parseNumbersInNumericColumns(schema)] });
  const data = xml[`${name}DataSet`];
  delete data["$"]; // Remove XML namespace property
  // return await data as StaticData;
  return {
    data,
    schema
  } as StaticData;
}

export const filterAvailableTables = (staticData: StaticData, config: Config): StaticData => {
  const data = Object.fromEntries(
    Object.entries(staticData.data).filter(([name, _]) => config.tables === null ? true : config.tables.indexOf(name) >= 0)
  );
  return { ...staticData, data };
}

export const getTable = (staticData: StaticData, config: Config): ((tableName: TableName) => Record<string, string | number | boolean | null>[] | undefined) => {
  const cachedTransformedData: { [tableName: string]: Record<string, string | number | boolean | null>[]; } = {};

  const lookupOriginalTable = (tableName: string): Record<string, string | number | boolean | null>[] => {
    switch (config.table_name_casing) {
      case "pascal_case":
        return staticData.data[tableName];
      case "lowercase":
        const name = Object.keys(staticData).find(originalTableName => originalTableName.toLowerCase() === tableName);
        if (name == undefined) throw new Error(`Unknown table name: ${tableName}`);
        return staticData.data[name];
      default:
        return unreachable(config.table_name_casing);
    }
  };

  const transformData = (tableData: Record<string, string | number | boolean | null>[]): Record<string, string | number | boolean | null>[] => {
    switch (config.column_name_casing) {
      case "pascal_case":
        return tableData;
      case "lowercase":
        return tableData.map(row => mapObject(row, ([column, value]) => [column.toLowerCase(), value]));
      default:
        return unreachable(config.column_name_casing);
    }
  };

  const lookupTable = (tableName: string): Record<string, string | number | boolean | null>[] => {
    const cachedData = cachedTransformedData[tableName];
    if (cachedData !== undefined)
      return cachedData;

    cachedTransformedData[tableName] = transformData(lookupOriginalTable(tableName));
    return cachedTransformedData[tableName];
  };

  return (tableName) => {
    if (config.schema) {
      return tableName.length === 2 && tableName[0] === config.schema
        ? lookupTable(tableName[1])
        : undefined;
    } else {
      return tableName.length === 1
        ? lookupTable(tableName[0])
        : undefined;
    }
  };
}

const applyCasing = (casing: Casing) => (str: string): string => {
  switch (casing) {
    case "pascal_case": return str;
    case "lowercase": return str.toLowerCase();
    default: return unreachable(casing);
  }
}

export const getSchema = (store: Record<string, StaticData>, config: Config, request: SchemaRequest = {}): SchemaResponse => {
  const applyTableNameCasing = applyCasing(config.table_name_casing);
  const applyColumnNameCasing = applyCasing(config.column_name_casing);

  const prefixSchemaToTableName = (tableName: TableName) =>
    config.schema
      ? [config.schema, ...tableName]
      : tableName;

  const dbName = config.db ? getDbStoreName(config.db) : defaultDbStoreName;
  const schema = store[dbName]?.schema;

  if(!schema) {
    throw new Error(`Couldn't find db store for ${dbName}`);
  }

  const filterForOnlyTheseTables = request.filters?.only_tables
    // If we're using a schema, only use those table names that belong to that schema
    ?.filter(n => config.schema ? n.length === 2 && n[0] === config.schema : true)
    // But the schema is fake, so just keep the actual table name
    ?.map(n => n[n.length - 1])

  const filteredTables = schema.tables.filter(table =>
    config.tables || filterForOnlyTheseTables
      ? (config.tables ?? []).concat(filterForOnlyTheseTables ?? []).map(n => [n]).find(nameEquals(table.name)) !== undefined
      : true
  );

  const prefixedTables: TableInfo[] = filteredTables.map(table => ({
    ...table,
    name: prefixSchemaToTableName(table.name.map(applyTableNameCasing)),
    primary_key: table.primary_key?.map(applyColumnNameCasing),
    foreign_keys: table.foreign_keys
      ? mapObjectValues(table.foreign_keys, constraint => ({
        ...constraint,
        foreign_table: prefixSchemaToTableName(constraint.foreign_table.map(applyTableNameCasing)),
        column_mapping: mapObject(constraint.column_mapping as Record<string, string>, ([outer, inner]) => [applyColumnNameCasing(outer), applyColumnNameCasing(inner)])
      }))
      : table.foreign_keys,
    columns: table.columns?.map(column => ({
      ...column,
      name: applyColumnNameCasing(column.name),
    }))
  }));

  const filterForOnlyTheseFunctions = request.filters?.only_functions
    // If we're using a schema, only use those function names that belong to that schema
    ?.filter(n => config.schema ? n.length === 2 && n[0] === config.schema : true)
    // But the schema is fake, so just keep the actual function name
    ?.map(n => n[n.length - 1])

  const filteredFunctions = (schema.functions ?? []).filter(func =>
    filterForOnlyTheseFunctions
      ? filterForOnlyTheseFunctions.map(n => [n]).find(nameEquals(func.name)) !== undefined
      : true
  )

  const prefixedFunctions = filteredFunctions; // TODO: Put some real prefixes here

  const detailLevel = request?.detail_level ?? "everything";
  switch (detailLevel) {
    case "everything":
      return {
        tables: prefixedTables,
        functions: prefixedFunctions,
      };

    case "basic_info":
      return {
        tables: prefixedTables.map(table => ({
          name: table.name,
          type: table.type
        })),
        functions: prefixedFunctions.map(func => ({
          name: func.name,
          type: func.type,
        })),
      };

    default:
      return unreachable(detailLevel);
  }


};
