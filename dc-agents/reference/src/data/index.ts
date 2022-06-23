import { SchemaResponse, ScalarType } from "../types/schema"
import { Config } from "../config";
import xml2js from "xml2js"
import fs from "fs"
import stream from "stream"
import zlib from "zlib"
import { parseNumbers } from "xml2js/lib/processors";

export type StaticData = {
  [tableName: string]: Record<string, string | number | boolean | null>[]
}

const streamToBuffer = async (stream : stream.Readable) : Promise<Buffer> => {
  const chunks = [];
  for await (const chunk of stream) {
      chunks.push(Buffer.from(chunk));
  }
  return Buffer.concat(chunks);
}

export const loadStaticData = async (): Promise<StaticData> => {
  const gzipReadStream = fs.createReadStream(__dirname + "/ChinookData.xml.gz");
  const unzipStream = stream.pipeline(gzipReadStream, zlib.createGunzip(), () => {});
  const xmlStr = (await streamToBuffer(unzipStream)).toString("utf16le");
  const xml = await xml2js.parseStringPromise(xmlStr, { explicitArray: false, valueProcessors: [parseNumbers] });
  const data = xml.ChinookDataSet;
  delete data["$"]; // Remove XML namespace property
  return await data as StaticData;
}

export const filterAvailableTables = (staticData: StaticData, config : Config): StaticData => {
  return Object.fromEntries(
    Object.entries(staticData).filter(([name, _]) => config.tables === null ? true : config.tables.indexOf(name) >= 0)
  );
}

const schema: SchemaResponse = {
  tables: [
    {
      name: "Artist",
      primary_key: "ArtistId",
      description: "Collection of artists of music",
      columns: [
        {
          name: "ArtistId",
          type: ScalarType.Number,
          nullable: false,
          description: "Artist primary key identifier"
        },
        {
          name: "Name",
          type: ScalarType.String,
          nullable: true,
          description: "The name of the artist"
        }
      ]
    },
    {
      name: "Album",
      primary_key: "AlbumId",
      description: "Collection of music albums created by artists",
      columns: [
        {
          name: "AlbumId",
          type: ScalarType.Number,
          nullable: false,
          description: "Album primary key identifier"
        },
        {
          name: "Title",
          type: ScalarType.String,
          nullable: false,
          description: "The title of the album"
        },
        {
          name: "ArtistId",
          type: ScalarType.Number,
          nullable: false,
          description: "The ID of the artist that created this album"
        }
      ]
    },
    {
      name: "Customer",
      primary_key: "CustomerId",
      description: "Collection of customers who can buy tracks",
      columns: [
        {
          name: "CustomerId",
          type: ScalarType.Number,
          nullable: false,
          description: "Customer primary key identifier"
        },
        {
          name: "FirstName",
          type: ScalarType.String,
          nullable: false,
          description: "The customer's first name"
        },
        {
          name: "LastName",
          type: ScalarType.String,
          nullable: false,
          description: "The customer's last name"
        },
        {
          name: "Company",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's company name"
        },
        {
          name: "Address",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's address line (street number, street)"
        },
        {
          name: "City",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's address city"
        },
        {
          name: "State",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's address state"
        },
        {
          name: "Country",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's address country"
        },
        {
          name: "PostalCode",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's address postal code"
        },
        {
          name: "Phone",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's phone number"
        },
        {
          name: "Fax",
          type: ScalarType.String,
          nullable: true,
          description: "The customer's fax number"
        },
        {
          name: "Email",
          type: ScalarType.String,
          nullable: false,
          description: "The customer's email address"
        },
        {
          name: "SupportRepId",
          type: ScalarType.Number,
          nullable: true,
          description: "The ID of the Employee who is this customer's support representative"
        }
      ]
    },
    {
      name: "Employee",
      primary_key: "EmployeeId",
      description: "Collection of employees who work for the business",
      columns: [
        {
          name: "EmployeeId",
          type: ScalarType.Number,
          nullable: false,
          description: "Employee primary key identifier"
        },
        {
          name: "FirstName",
          type: ScalarType.String,
          nullable: false,
          description: "The employee's first name"
        },
        {
          name: "LastName",
          type: ScalarType.String,
          nullable: false,
          description: "The employee's last name"
        },
        {
          name: "Title",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's job title"
        },
        {
          name: "BirthDate",
          type: ScalarType.String, // Ought to be DateTime but we don't have a type for this yet
          nullable: true,
          description: "The employee's birth date"
        },
        {
          name: "HireDate",
          type: ScalarType.String, // Ought to be DateTime but we don't have a type for this yet
          nullable: true,
          description: "The employee's birth date"
        },
        {
          name: "Address",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's address line (street number, street)"
        },
        {
          name: "City",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's address city"
        },
        {
          name: "State",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's address state"
        },
        {
          name: "Country",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's address country"
        },
        {
          name: "PostalCode",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's address postal code"
        },
        {
          name: "Phone",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's phone number"
        },
        {
          name: "Fax",
          type: ScalarType.String,
          nullable: true,
          description: "The employee's fax number"
        },
        {
          name: "Email",
          type: ScalarType.String,
          nullable: false,
          description: "The employee's email address"
        },
      ]
    },
    {
      name: "Genre",
      primary_key: "GenreId",
      description: "Genres of music",
      columns: [
        {
          name: "GenreId",
          type: ScalarType.Number,
          nullable: false,
          description: "Genre primary key identifier"
        },
        {
          name: "Name",
          type: ScalarType.String,
          nullable: true,
          description: "The name of the genre"
        }
      ]
    },
    {
      name: "Invoice",
      primary_key: "InvoiceId",
      description: "Collection of invoices of music purchases by a customer",
      columns: [
        {
          name: "InvoiceId",
          type: ScalarType.Number,
          nullable: false,
          description: "Invoice primary key identifier"
        },
        {
          name: "CustomerId",
          type: ScalarType.Number,
          nullable: false,
          description: "ID of the customer who bought the music"
        },
        {
          name: "InvoiceDate",
          type: ScalarType.String, // Ought to be DateTime but we don't have a type for this yet
          nullable: false,
          description: "Date of the invoice"
        },
        {
          name: "BillingAddress",
          type: ScalarType.String,
          nullable: true,
          description: "The invoice's billing address line (street number, street)"
        },
        {
          name: "BillingCity",
          type: ScalarType.String,
          nullable: true,
          description: "The invoice's billing address city"
        },
        {
          name: "BillingState",
          type: ScalarType.String,
          nullable: true,
          description: "The invoice's billing address state"
        },
        {
          name: "BillingCountry",
          type: ScalarType.String,
          nullable: true,
          description: "The invoice's billing address country"
        },
        {
          name: "BillingPostalCode",
          type: ScalarType.String,
          nullable: true,
          description: "The invoice's billing address postal code"
        },
        {
          name: "Total",
          type: ScalarType.Number,
          nullable: false,
          description: "The total amount due on the invoice"
        },
      ]
    },
    {
      name: "InvoiceLine",
      primary_key: "InvoiceLineId",
      description: "Collection of track purchasing line items of invoices",
      columns: [
        {
          name: "InvoiceLineId",
          type: ScalarType.Number,
          nullable: false,
          description: "Invoice Line primary key identifier"
        },
        {
          name: "InvoiceId",
          type: ScalarType.Number,
          nullable: false,
          description: "ID of the invoice the line belongs to"
        },
        {
          name: "TrackId",
          type: ScalarType.Number,
          nullable: false,
          description: "ID of the music track being purchased"
        },
        {
          name: "UnitPrice",
          type: ScalarType.Number,
          nullable: false,
          description: "Price of each individual track unit"
        },
        {
          name: "Quantity",
          type: ScalarType.Number,
          nullable: false,
          description: "Quantity of the track purchased"
        },
      ]
    },
    {
      name: "MediaType",
      primary_key: "MediaTypeId",
      description: "Collection of media types that tracks can be encoded in",
      columns: [
        {
          name: "MediaTypeId",
          type: ScalarType.Number,
          nullable: false,
          description: "Media Type primary key identifier"
        },
        {
          name: "Name",
          type: ScalarType.Number,
          nullable: true,
          description: "The name of the media type format"
        },
      ]
    },
    {
      name: "Playlist",
      primary_key: "PlaylistId",
      description: "Collection of playlists",
      columns: [
        {
          name: "PlaylistId",
          type: ScalarType.Number,
          nullable: false,
          description: "Playlist primary key identifier"
        },
        {
          name: "Name",
          type: ScalarType.Number,
          nullable: true,
          description: "The name of the playlist"
        },
      ]
    },
    // We don't support composite primary keys yet :(
    // {
    //   name: "PlaylistTrack",
    //   primary_key: ["PlaylistId", "TrackId"],
    //   description: "Associations between playlists and tracks",
    //   columns: [
    //     {
    //       name: "PlaylistId",
    //       type: ScalarType.Number,
    //       nullable: false,
    //       description: "The ID of the playlist"
    //     },
    //     {
    //       name: "TrackId",
    //       type: ScalarType.Number,
    //       nullable: true,
    //       description: "The ID of the track"
    //     },
    //   ]
    // },
    {
      name: "Track",
      primary_key: "TrackId",
      description: "Collection of music tracks",
      columns: [
        {
          name: "TrackId",
          type: ScalarType.Number,
          nullable: false,
          description: "The ID of the playlist"
        },
        {
          name: "Name",
          type: ScalarType.String,
          nullable: false,
          description: "The name of the track"
        },
        {
          name: "AlbumId",
          type: ScalarType.Number,
          nullable: true,
          description: "The ID of the album the track belongs to"
        },
        {
          name: "MediaTypeId",
          type: ScalarType.Number,
          nullable: true,
          description: "The ID of the media type the track is encoded with"
        },
        {
          name: "GenreId",
          type: ScalarType.Number,
          nullable: true,
          description: "The ID of the genre of the track"
        },
        {
          name: "Composer",
          type: ScalarType.String,
          nullable: true,
          description: "The name of the composer of the track"
        },
        {
          name: "Milliseconds",
          type: ScalarType.Number,
          nullable: false,
          description: "The length of the track in milliseconds"
        },
        {
          name: "Bytes",
          type: ScalarType.Number,
          nullable: true,
          description: "The size of the track in bytes"
        },
        {
          name: "UnitPrice",
          type: ScalarType.Number,
          nullable: false,
          description: "The price of the track"
        },
      ]
    },
  ]
};

export const getSchema = (config: Config): SchemaResponse => {
  return {
    ...schema,
    tables: schema.tables.filter(table => config.tables === null ? true : config.tables.indexOf(table.name) >= 0)
  };
};
