import { SchemaResponse, TableName } from "@hasura/dc-api-types"
import { Config } from "../config";
import xml2js from "xml2js"
import fs from "fs"
import stream from "stream"
import zlib from "zlib"
import { parseNumbers } from "xml2js/lib/processors";
import { tableNameEquals } from "../util";

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

export const loadStaticData = async (): Promise<StaticData> => {
  const gzipReadStream = fs.createReadStream(__dirname + "/ChinookData.xml.gz");
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

export const getTable = (staticData: StaticData, config: Config) => (tableName: TableName): Record<string, string | number | boolean | null>[] | undefined => {
  if (config.schema) {
    return tableName.length === 2 && tableName[0] === config.schema
      ? staticData[tableName[1]]
      : undefined;
  } else {
    return tableName.length === 1
      ? staticData[tableName[0]]
      : undefined;
  }
}

const schema: SchemaResponse = {
  tables: [
    {
      name: ["Artist"],
      primary_key: ["ArtistId"],
      description: "Collection of artists of music",
      columns: [
        {
          name: "ArtistId",
          type: "number",
          nullable: false,
          description: "Artist primary key identifier"
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the artist"
        }
      ]
    },
    {
      name: ["Album"],
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
          description: "Album primary key identifier"
        },
        {
          name: "Title",
          type: "string",
          nullable: false,
          description: "The title of the album"
        },
        {
          name: "ArtistId",
          type: "number",
          nullable: false,
          description: "The ID of the artist that created this album"
        }
      ]
    },
    {
      name: ["Customer"],
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
          description: "Customer primary key identifier"
        },
        {
          name: "FirstName",
          type: "string",
          nullable: false,
          description: "The customer's first name"
        },
        {
          name: "LastName",
          type: "string",
          nullable: false,
          description: "The customer's last name"
        },
        {
          name: "Company",
          type: "string",
          nullable: true,
          description: "The customer's company name"
        },
        {
          name: "Address",
          type: "string",
          nullable: true,
          description: "The customer's address line (street number, street)"
        },
        {
          name: "City",
          type: "string",
          nullable: true,
          description: "The customer's address city"
        },
        {
          name: "State",
          type: "string",
          nullable: true,
          description: "The customer's address state"
        },
        {
          name: "Country",
          type: "string",
          nullable: true,
          description: "The customer's address country"
        },
        {
          name: "PostalCode",
          type: "string",
          nullable: true,
          description: "The customer's address postal code"
        },
        {
          name: "Phone",
          type: "string",
          nullable: true,
          description: "The customer's phone number"
        },
        {
          name: "Fax",
          type: "string",
          nullable: true,
          description: "The customer's fax number"
        },
        {
          name: "Email",
          type: "string",
          nullable: false,
          description: "The customer's email address"
        },
        {
          name: "SupportRepId",
          type: "number",
          nullable: true,
          description: "The ID of the Employee who is this customer's support representative"
        }
      ]
    },
    {
      name: ["Employee"],
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
          description: "Employee primary key identifier"
        },
        {
          name: "LastName",
          type: "string",
          nullable: false,
          description: "The employee's last name"
        },
        {
          name: "FirstName",
          type: "string",
          nullable: false,
          description: "The employee's first name"
        },
        {
          name: "Title",
          type: "string",
          nullable: true,
          description: "The employee's job title"
        },
        {
          name: "ReportsTo",
          type: "number",
          nullable: true,
          description: "The employee's manager"
        },
        {
          name: "BirthDate",
          type: "DateTime",
          nullable: true,
          description: "The employee's birth date"
        },
        {
          name: "HireDate",
          type: "DateTime",
          nullable: true,
          description: "The employee's hire date"
        },
        {
          name: "Address",
          type: "string",
          nullable: true,
          description: "The employee's address line (street number, street)"
        },
        {
          name: "City",
          type: "string",
          nullable: true,
          description: "The employee's address city"
        },
        {
          name: "State",
          type: "string",
          nullable: true,
          description: "The employee's address state"
        },
        {
          name: "Country",
          type: "string",
          nullable: true,
          description: "The employee's address country"
        },
        {
          name: "PostalCode",
          type: "string",
          nullable: true,
          description: "The employee's address postal code"
        },
        {
          name: "Phone",
          type: "string",
          nullable: true,
          description: "The employee's phone number"
        },
        {
          name: "Fax",
          type: "string",
          nullable: true,
          description: "The employee's fax number"
        },
        {
          name: "Email",
          type: "string",
          nullable: true,
          description: "The employee's email address"
        },
      ]
    },
    {
      name: ["Genre"],
      primary_key: ["GenreId"],
      description: "Genres of music",
      columns: [
        {
          name: "GenreId",
          type: "number",
          nullable: false,
          description: "Genre primary key identifier"
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the genre"
        }
      ]
    },
    {
      name: ["Invoice"],
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
          description: "Invoice primary key identifier"
        },
        {
          name: "CustomerId",
          type: "number",
          nullable: false,
          description: "ID of the customer who bought the music"
        },
        {
          name: "InvoiceDate",
          type: "DateTime",
          nullable: false,
          description: "Date of the invoice"
        },
        {
          name: "BillingAddress",
          type: "string",
          nullable: true,
          description: "The invoice's billing address line (street number, street)"
        },
        {
          name: "BillingCity",
          type: "string",
          nullable: true,
          description: "The invoice's billing address city"
        },
        {
          name: "BillingState",
          type: "string",
          nullable: true,
          description: "The invoice's billing address state"
        },
        {
          name: "BillingCountry",
          type: "string",
          nullable: true,
          description: "The invoice's billing address country"
        },
        {
          name: "BillingPostalCode",
          type: "string",
          nullable: true,
          description: "The invoice's billing address postal code"
        },
        {
          name: "Total",
          type: "number",
          nullable: false,
          description: "The total amount due on the invoice"
        },
      ]
    },
    {
      name: ["InvoiceLine"],
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
          description: "Invoice Line primary key identifier"
        },
        {
          name: "InvoiceId",
          type: "number",
          nullable: false,
          description: "ID of the invoice the line belongs to"
        },
        {
          name: "TrackId",
          type: "number",
          nullable: false,
          description: "ID of the music track being purchased"
        },
        {
          name: "UnitPrice",
          type: "number",
          nullable: false,
          description: "Price of each individual track unit"
        },
        {
          name: "Quantity",
          type: "number",
          nullable: false,
          description: "Quantity of the track purchased"
        },
      ]
    },
    {
      name: ["MediaType"],
      primary_key: ["MediaTypeId"],
      description: "Collection of media types that tracks can be encoded in",
      columns: [
        {
          name: "MediaTypeId",
          type: "number",
          nullable: false,
          description: "Media Type primary key identifier"
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the media type format"
        },
      ]
    },
    {
      name: ["Playlist"],
      primary_key: ["PlaylistId"],
      description: "Collection of playlists",
      columns: [
        {
          name: "PlaylistId",
          type: "number",
          nullable: false,
          description: "Playlist primary key identifier"
        },
        {
          name: "Name",
          type: "string",
          nullable: true,
          description: "The name of the playlist"
        },
      ]
    },
    {
      name: ["PlaylistTrack"],
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
          description: "The ID of the playlist"
        },
        {
          name: "TrackId",
          type: "number",
          nullable: false,
          description: "The ID of the track"
        },
      ]
    },
    {
      name: ["Track"],
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
          description: "The ID of the track"
        },
        {
          name: "Name",
          type: "string",
          nullable: false,
          description: "The name of the track"
        },
        {
          name: "AlbumId",
          type: "number",
          nullable: true,
          description: "The ID of the album the track belongs to"
        },
        {
          name: "MediaTypeId",
          type: "number",
          nullable: false,
          description: "The ID of the media type the track is encoded with"
        },
        {
          name: "GenreId",
          type: "number",
          nullable: true,
          description: "The ID of the genre of the track"
        },
        {
          name: "Composer",
          type: "string",
          nullable: true,
          description: "The name of the composer of the track"
        },
        {
          name: "Milliseconds",
          type: "number",
          nullable: false,
          description: "The length of the track in milliseconds"
        },
        {
          name: "Bytes",
          type: "number",
          nullable: true,
          description: "The size of the track in bytes"
        },
        {
          name: "UnitPrice",
          type: "number",
          nullable: false,
          description: "The price of the track"
        },
      ]
    },
  ]
};

export const getSchema = (config: Config): SchemaResponse => {
  const prefixSchemaToTableName = (tableName: TableName) =>
    config.schema
      ? [config.schema, ...tableName]
      : tableName;

  const filteredTables = schema.tables.filter(table =>
    config.tables === null ? true : config.tables.map(n => [n]).find(tableNameEquals(table.name)) !== undefined
  );

  const prefixedTables = filteredTables.map(table => ({
    ...table,
    name: prefixSchemaToTableName(table.name),
  }));

  return {
    ...schema,
    tables: prefixedTables
  };
};
