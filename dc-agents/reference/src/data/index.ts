import { SchemaResponse, TableName } from "@hasura/dc-api-types"
import { Casing, Config } from "../config";
import xml2js from "xml2js"
import fs from "fs"
import stream from "stream"
import zlib from "zlib"
import { parseNumbers } from "xml2js/lib/processors";
import { mapObject, mapObjectValues, tableNameEquals, unreachable } from "../util";

export type StaticData = {
  [tableName: string]: Record<string, string | number | boolean | null>[]
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
  const numericColumns = new Set(schema.tables.flatMap(table => table.columns.filter(c => c.type === "number").map(c => c.name)));

  return (value: string, name: string): any => {
    return numericColumns.has(name)
      ? parseNumbers(value)
      : value;
  };
}

export const loadStaticData = async (name: string): Promise<StaticData> => {
  const gzipReadStream = fs.createReadStream(__dirname + "/" + name);
  const unzipStream = stream.pipeline(gzipReadStream, zlib.createGunzip(), () => { });
  const xmlStr = (await streamToBuffer(unzipStream)).toString("utf16le");
  const xml = await xml2js.parseStringPromise(xmlStr, { explicitArray: false, emptyTag: () => null, valueProcessors: [parseNumbersInNumericColumns(schema)] });
  const data = xml.ChinookDataSet;
  delete data["$"]; // Remove XML namespace property
  return await data as StaticData;
}

export const filterAvailableTables = (staticData: StaticData, config: Config): StaticData => {
  return Object.fromEntries(
    Object.entries(staticData).filter(([name, _]) => config.tables === null ? true : config.tables.indexOf(name) >= 0)
  );
}

export const getTable = (staticData: StaticData, config: Config): ((tableName: TableName) => Record<string, string | number | boolean | null>[] | undefined) => {
  const cachedTransformedData: StaticData = {};

  const lookupOriginalTable = (tableName: string): Record<string, string | number | boolean | null>[] => {
    switch (config.table_name_casing) {
      case "pascal_case":
        return staticData[tableName];
      case "lowercase":
        const name = Object.keys(staticData).find(originalTableName => originalTableName.toLowerCase() === tableName);
        if (name == undefined) throw new Error(`Unknown table name: ${tableName}`);
        return staticData[name];
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

const schema: SchemaResponse = {
  tables: [
    {
      name: ["Artist"],
      type: "table",
      primary_key: ["ArtistId"],
      description: "Collection of artists of music",
      columns: [
        {
          name: "ArtistId",
          type: "number",
          nullable: false,
          description: "Artist primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the artist",
          insertable: false,
          updatable: false,
        }
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Album"],
      type: "table",
      primary_key: ["AlbumId"],
      foreign_keys: {
        "Artist": {
          column_mapping: {
            "ArtistId": "ArtistId"
          },
          foreign_table: ["Artist"],
        }
      },
      description: "Collection of music albums created by artists",
      columns: [
        {
          name: "AlbumId",
          type: "number",
          nullable: false,
          description: "Album primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "Title",
          type: "string",
          nullable: false,
          description: "The title of the album",
          insertable: false,
          updatable: false,
        },
        {
          name: "ArtistId",
          type: "number",
          nullable: false,
          description: "The ID of the artist that created this album",
          insertable: false,
          updatable: false,
        }
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Customer"],
      type: "table",
      primary_key: ["CustomerId"],
      foreign_keys: {
        "CustomerSupportRep": {
          column_mapping: {
            "SupportRepId": "EmployeeId"
          },
          foreign_table: ["Employee"],
        }
      },
      description: "Collection of customers who can buy tracks",
      columns: [
        {
          name: "CustomerId",
          type: "number",
          nullable: false,
          description: "Customer primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "FirstName",
          type: "string",
          nullable: false,
          description: "The customer's first name",
          insertable: false,
          updatable: false,
        },
        {
          name: "LastName",
          type: "string",
          nullable: false,
          description: "The customer's last name",
          insertable: false,
          updatable: false,
        },
        {
          name: "Company",
          type: "string",
          nullable: true,
          description: "The customer's company name",
          insertable: false,
          updatable: false,
        },
        {
          name: "Address",
          type: "string",
          nullable: true,
          description: "The customer's address line (street number, street)",
          insertable: false,
          updatable: false,
        },
        {
          name: "City",
          type: "string",
          nullable: true,
          description: "The customer's address city",
          insertable: false,
          updatable: false,
        },
        {
          name: "State",
          type: "string",
          nullable: true,
          description: "The customer's address state",
          insertable: false,
          updatable: false,
        },
        {
          name: "Country",
          type: "string",
          nullable: true,
          description: "The customer's address country",
          insertable: false,
          updatable: false,
        },
        {
          name: "PostalCode",
          type: "string",
          nullable: true,
          description: "The customer's address postal code",
          insertable: false,
          updatable: false,
        },
        {
          name: "Phone",
          type: "string",
          nullable: true,
          description: "The customer's phone number",
          insertable: false,
          updatable: false,
        },
        {
          name: "Fax",
          type: "string",
          nullable: true,
          description: "The customer's fax number",
          insertable: false,
          updatable: false,
        },
        {
          name: "Email",
          type: "string",
          nullable: false,
          description: "The customer's email address",
          insertable: false,
          updatable: false,
        },
        {
          name: "SupportRepId",
          type: "number",
          nullable: true,
          description: "The ID of the Employee who is this customer's support representative",
          insertable: false,
          updatable: false,
        }
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Employee"],
      type: "table",
      primary_key: ["EmployeeId"],
      foreign_keys: {
        "EmployeeReportsTo": {
          column_mapping: {
            "ReportsTo": "EmployeeId"
          },
          foreign_table: ["Employee"],
        }
      },
      description: "Collection of employees who work for the business",
      columns: [
        {
          name: "EmployeeId",
          type: "number",
          nullable: false,
          description: "Employee primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "LastName",
          type: "string",
          nullable: false,
          description: "The employee's last name",
          insertable: false,
          updatable: false,
        },
        {
          name: "FirstName",
          type: "string",
          nullable: false,
          description: "The employee's first name",
          insertable: false,
          updatable: false,
        },
        {
          name: "Title",
          type: "string",
          nullable: true,
          description: "The employee's job title",
          insertable: false,
          updatable: false,
        },
        {
          name: "ReportsTo",
          type: "number",
          nullable: true,
          description: "The employee's manager",
          insertable: false,
          updatable: false,
        },
        {
          name: "BirthDate",
          type: "DateTime",
          nullable: true,
          description: "The employee's birth date",
          insertable: false,
          updatable: false,
        },
        {
          name: "HireDate",
          type: "DateTime",
          nullable: true,
          description: "The employee's hire date",
          insertable: false,
          updatable: false,
        },
        {
          name: "Address",
          type: "string",
          nullable: true,
          description: "The employee's address line (street number, street)",
          insertable: false,
          updatable: false,
        },
        {
          name: "City",
          type: "string",
          nullable: true,
          description: "The employee's address city",
          insertable: false,
          updatable: false,
        },
        {
          name: "State",
          type: "string",
          nullable: true,
          description: "The employee's address state",
          insertable: false,
          updatable: false,
        },
        {
          name: "Country",
          type: "string",
          nullable: true,
          description: "The employee's address country",
          insertable: false,
          updatable: false,
        },
        {
          name: "PostalCode",
          type: "string",
          nullable: true,
          description: "The employee's address postal code",
          insertable: false,
          updatable: false,
        },
        {
          name: "Phone",
          type: "string",
          nullable: true,
          description: "The employee's phone number",
          insertable: false,
          updatable: false,
        },
        {
          name: "Fax",
          type: "string",
          nullable: true,
          description: "The employee's fax number",
          insertable: false,
          updatable: false,
        },
        {
          name: "Email",
          type: "string",
          nullable: true,
          description: "The employee's email address",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Genre"],
      type: "table",
      primary_key: ["GenreId"],
      description: "Genres of music",
      columns: [
        {
          name: "GenreId",
          type: "number",
          nullable: false,
          description: "Genre primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the genre",
          insertable: false,
          updatable: false,
        }
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Invoice"],
      type: "table",
      primary_key: ["InvoiceId"],
      foreign_keys: {
        "InvoiceCustomer": {
          column_mapping: {
            "CustomerId": "CustomerId"
          },
          foreign_table: ["Customer"],
        }
      },
      description: "Collection of invoices of music purchases by a customer",
      columns: [
        {
          name: "InvoiceId",
          type: "number",
          nullable: false,
          description: "Invoice primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "CustomerId",
          type: "number",
          nullable: false,
          description: "ID of the customer who bought the music",
          insertable: false,
          updatable: false,
        },
        {
          name: "InvoiceDate",
          type: "DateTime",
          nullable: false,
          description: "Date of the invoice",
          insertable: false,
          updatable: false,
        },
        {
          name: "BillingAddress",
          type: "string",
          nullable: true,
          description: "The invoice's billing address line (street number, street)",
          insertable: false,
          updatable: false,
        },
        {
          name: "BillingCity",
          type: "string",
          nullable: true,
          description: "The invoice's billing address city",
          insertable: false,
          updatable: false,
        },
        {
          name: "BillingState",
          type: "string",
          nullable: true,
          description: "The invoice's billing address state",
          insertable: false,
          updatable: false,
        },
        {
          name: "BillingCountry",
          type: "string",
          nullable: true,
          description: "The invoice's billing address country",
          insertable: false,
          updatable: false,
        },
        {
          name: "BillingPostalCode",
          type: "string",
          nullable: true,
          description: "The invoice's billing address postal code",
          insertable: false,
          updatable: false,
        },
        {
          name: "Total",
          type: "number",
          nullable: false,
          description: "The total amount due on the invoice",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["InvoiceLine"],
      type: "table",
      primary_key: ["InvoiceLineId"],
      foreign_keys: {
        "Invoice": {
          column_mapping: {
            "InvoiceId": "InvoiceId"
          },
          foreign_table: ["Invoice"],
        },
        "Track": {
          column_mapping: {
            "TrackId": "TrackId"
          },
          foreign_table: ["Track"],
        }
      },
      description: "Collection of track purchasing line items of invoices",
      columns: [
        {
          name: "InvoiceLineId",
          type: "number",
          nullable: false,
          description: "Invoice Line primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "InvoiceId",
          type: "number",
          nullable: false,
          description: "ID of the invoice the line belongs to",
          insertable: false,
          updatable: false,
        },
        {
          name: "TrackId",
          type: "number",
          nullable: false,
          description: "ID of the music track being purchased",
          insertable: false,
          updatable: false,
        },
        {
          name: "UnitPrice",
          type: "number",
          nullable: false,
          description: "Price of each individual track unit",
          insertable: false,
          updatable: false,
        },
        {
          name: "Quantity",
          type: "number",
          nullable: false,
          description: "Quantity of the track purchased",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["MediaType"],
      type: "table",
      primary_key: ["MediaTypeId"],
      description: "Collection of media types that tracks can be encoded in",
      columns: [
        {
          name: "MediaTypeId",
          type: "number",
          nullable: false,
          description: "Media Type primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the media type format",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Playlist"],
      type: "table",
      primary_key: ["PlaylistId"],
      description: "Collection of playlists",
      columns: [
        {
          name: "PlaylistId",
          type: "number",
          nullable: false,
          description: "Playlist primary key identifier",
          insertable: false,
          updatable: false,
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the playlist",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["PlaylistTrack"],
      type: "table",
      primary_key: ["PlaylistId", "TrackId"],
      foreign_keys: {
        "Playlist": {
          column_mapping: {
            "PlaylistId": "PlaylistId"
          },
          foreign_table: ["Playlist"],
        },
        "Track": {
          column_mapping: {
            "TrackId": "TrackId"
          },
          foreign_table: ["Track"],
        }
      },
      description: "Associations between playlists and tracks",
      columns: [
        {
          name: "PlaylistId",
          type: "number",
          nullable: false,
          description: "The ID of the playlist",
          insertable: false,
          updatable: false,
        },
        {
          name: "TrackId",
          type: "number",
          nullable: false,
          description: "The ID of the track",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
    {
      name: ["Track"],
      type: "table",
      primary_key: ["TrackId"],
      foreign_keys: {
        "Album": {
          column_mapping: {
            "AlbumId": "AlbumId"
          },
          foreign_table: ["Album"],
        },
        "Genre": {
          column_mapping: {
            "GenreId": "GenreId"
          },
          foreign_table: ["Genre"],
        },
        "MediaType": {
          column_mapping: {
            "MediaTypeId": "MediaTypeId"
          },
          foreign_table: ["MediaType"],
        }
      },
      description: "Collection of music tracks",
      columns: [
        {
          name: "TrackId",
          type: "number",
          nullable: false,
          description: "The ID of the track",
          insertable: false,
          updatable: false,
        },
        {
          name: "Name",
          type: "string",
          nullable: false,
          description: "The name of the track",
          insertable: false,
          updatable: false,
        },
        {
          name: "AlbumId",
          type: "number",
          nullable: true,
          description: "The ID of the album the track belongs to",
          insertable: false,
          updatable: false,
        },
        {
          name: "MediaTypeId",
          type: "number",
          nullable: false,
          description: "The ID of the media type the track is encoded with",
          insertable: false,
          updatable: false,
        },
        {
          name: "GenreId",
          type: "number",
          nullable: true,
          description: "The ID of the genre of the track",
          insertable: false,
          updatable: false,
        },
        {
          name: "Composer",
          type: "string",
          nullable: true,
          description: "The name of the composer of the track",
          insertable: false,
          updatable: false,
        },
        {
          name: "Milliseconds",
          type: "number",
          nullable: false,
          description: "The length of the track in milliseconds",
          insertable: false,
          updatable: false,
        },
        {
          name: "Bytes",
          type: "number",
          nullable: true,
          description: "The size of the track in bytes",
          insertable: false,
          updatable: false,
        },
        {
          name: "UnitPrice",
          type: "number",
          nullable: false,
          description: "The price of the track",
          insertable: false,
          updatable: false,
        },
      ],
      insertable: false,
      updatable: false,
      deletable: false,
    },
  ]
};

const applyCasing = (casing: Casing) => (str: string): string => {
  switch (casing) {
    case "pascal_case": return str;
    case "lowercase": return str.toLowerCase();
    default: return unreachable(casing);
  }
}

export const getSchema = (config: Config): SchemaResponse => {
  const applyTableNameCasing = applyCasing(config.table_name_casing);
  const applyColumnNameCasing = applyCasing(config.column_name_casing);

  const prefixSchemaToTableName = (tableName: TableName) =>
    config.schema
      ? [config.schema, ...tableName]
      : tableName;

  const filteredTables = schema.tables.filter(table =>
    config.tables === null ? true : config.tables.map(n => [n]).find(tableNameEquals(table.name)) !== undefined
  );

  const prefixedTables = filteredTables.map(table => ({
    ...table,
    name: prefixSchemaToTableName(table.name.map(applyTableNameCasing)),
    primary_key: table.primary_key?.map(applyColumnNameCasing),
    foreign_keys: table.foreign_keys
      ? mapObjectValues(table.foreign_keys, constraint => ({
        ...constraint,
        foreign_table: prefixSchemaToTableName(constraint.foreign_table.map(applyTableNameCasing)),
        column_mapping: mapObject(constraint.column_mapping, ([outer, inner]) => [applyColumnNameCasing(outer), applyColumnNameCasing(inner)])
      }))
      : table.foreign_keys,
    columns: table.columns.map(column => ({
      ...column,
      name: applyColumnNameCasing(column.name),
    }))
  }));

  return {
    ...schema,
    tables: prefixedTables
  };
};
