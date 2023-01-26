/*******************************************************************************
   Chinook Database - Version 1.4
   Script: Chinook_PostgreSql.sql
   Description: Creates and populates the Chinook database.
   DB Server: PostgreSql
   Author: Luis Rocha
   License: http://www.codeplex.com/ChinookDatabase/license
********************************************************************************/


/*******************************************************************************
   Create Tables
********************************************************************************/
CREATE TABLE "Album"
(
    "AlbumId" INT NOT NULL,
    "Title" VARCHAR(160) NOT NULL,
    "ArtistId" INT NOT NULL,
    CONSTRAINT "PK_Album" PRIMARY KEY  ("AlbumId")
);

CREATE TABLE "Artist"
(
    "ArtistId" INT NOT NULL,
    "Name" VARCHAR(120),
    CONSTRAINT "PK_Artist" PRIMARY KEY  ("ArtistId")
);

CREATE TABLE "Customer"
(
    "CustomerId" INT NOT NULL,
    "FirstName" VARCHAR(40) NOT NULL,
    "LastName" VARCHAR(20) NOT NULL,
    "Company" VARCHAR(80),
    "Address" VARCHAR(70),
    "City" VARCHAR(40),
    "State" VARCHAR(40),
    "Country" VARCHAR(40),
    "PostalCode" VARCHAR(10),
    "Phone" VARCHAR(24),
    "Fax" VARCHAR(24),
    "Email" VARCHAR(60) NOT NULL,
    "SupportRepId" INT,
    CONSTRAINT "PK_Customer" PRIMARY KEY  ("CustomerId")
);

CREATE TABLE "Employee"
(
    "EmployeeId" INT NOT NULL,
    "LastName" VARCHAR(20) NOT NULL,
    "FirstName" VARCHAR(20) NOT NULL,
    "Title" VARCHAR(30),
    "ReportsTo" INT,
    "BirthDate" TIMESTAMP,
    "HireDate" TIMESTAMP,
    "Address" VARCHAR(70),
    "City" VARCHAR(40),
    "State" VARCHAR(40),
    "Country" VARCHAR(40),
    "PostalCode" VARCHAR(10),
    "Phone" VARCHAR(24),
    "Fax" VARCHAR(24),
    "Email" VARCHAR(60),
    CONSTRAINT "PK_Employee" PRIMARY KEY  ("EmployeeId")
);

CREATE TABLE "Genre"
(
    "GenreId" INT NOT NULL,
    "Name" VARCHAR(120),
    CONSTRAINT "PK_Genre" PRIMARY KEY  ("GenreId")
);

CREATE TABLE "Invoice"
(
    "InvoiceId" INT NOT NULL,
    "CustomerId" INT NOT NULL,
    "InvoiceDate" TIMESTAMP NOT NULL,
    "BillingAddress" VARCHAR(70),
    "BillingCity" VARCHAR(40),
    "BillingState" VARCHAR(40),
    "BillingCountry" VARCHAR(40),
    "BillingPostalCode" VARCHAR(10),
    "Total" NUMERIC(10,2) NOT NULL,
    CONSTRAINT "PK_Invoice" PRIMARY KEY  ("InvoiceId")
);

CREATE TABLE "InvoiceLine"
(
    "InvoiceLineId" INT NOT NULL,
    "InvoiceId" INT NOT NULL,
    "TrackId" INT NOT NULL,
    "UnitPrice" NUMERIC(10,2) NOT NULL,
    "Quantity" INT NOT NULL,
    CONSTRAINT "PK_InvoiceLine" PRIMARY KEY  ("InvoiceLineId")
);

CREATE TABLE "MediaType"
(
    "MediaTypeId" INT NOT NULL,
    "Name" VARCHAR(120),
    CONSTRAINT "PK_MediaType" PRIMARY KEY  ("MediaTypeId")
);

CREATE TABLE "Playlist"
(
    "PlaylistId" INT NOT NULL,
    "Name" VARCHAR(120),
    CONSTRAINT "PK_Playlist" PRIMARY KEY  ("PlaylistId")
);

CREATE TABLE "PlaylistTrack"
(
    "PlaylistId" INT NOT NULL,
    "TrackId" INT NOT NULL,
    CONSTRAINT "PK_PlaylistTrack" PRIMARY KEY  ("PlaylistId", "TrackId")
);

CREATE TABLE "Track"
(
    "TrackId" INT NOT NULL,
    "Name" VARCHAR(200) NOT NULL,
    "AlbumId" INT,
    "MediaTypeId" INT NOT NULL,
    "GenreId" INT,
    "Composer" VARCHAR(220),
    "Milliseconds" INT NOT NULL,
    "Bytes" INT,
    "UnitPrice" NUMERIC(10,2) NOT NULL,
    CONSTRAINT "PK_Track" PRIMARY KEY  ("TrackId")
);



/*******************************************************************************
   Create Primary Key Unique Indexes
********************************************************************************/

/*******************************************************************************
   Create Foreign Keys
********************************************************************************/
ALTER TABLE "Album" ADD CONSTRAINT "FK_AlbumArtistId"
    FOREIGN KEY ("ArtistId") REFERENCES "Artist" ("ArtistId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_AlbumArtistId" ON "Album" ("ArtistId");

ALTER TABLE "Customer" ADD CONSTRAINT "FK_CustomerSupportRepId"
    FOREIGN KEY ("SupportRepId") REFERENCES "Employee" ("EmployeeId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_CustomerSupportRepId" ON "Customer" ("SupportRepId");

ALTER TABLE "Employee" ADD CONSTRAINT "FK_EmployeeReportsTo"
    FOREIGN KEY ("ReportsTo") REFERENCES "Employee" ("EmployeeId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_EmployeeReportsTo" ON "Employee" ("ReportsTo");

ALTER TABLE "Invoice" ADD CONSTRAINT "FK_InvoiceCustomerId"
    FOREIGN KEY ("CustomerId") REFERENCES "Customer" ("CustomerId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_InvoiceCustomerId" ON "Invoice" ("CustomerId");

ALTER TABLE "InvoiceLine" ADD CONSTRAINT "FK_InvoiceLineInvoiceId"
    FOREIGN KEY ("InvoiceId") REFERENCES "Invoice" ("InvoiceId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_InvoiceLineInvoiceId" ON "InvoiceLine" ("InvoiceId");

ALTER TABLE "InvoiceLine" ADD CONSTRAINT "FK_InvoiceLineTrackId"
    FOREIGN KEY ("TrackId") REFERENCES "Track" ("TrackId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_InvoiceLineTrackId" ON "InvoiceLine" ("TrackId");

ALTER TABLE "PlaylistTrack" ADD CONSTRAINT "FK_PlaylistTrackPlaylistId"
    FOREIGN KEY ("PlaylistId") REFERENCES "Playlist" ("PlaylistId") ON DELETE NO ACTION ON UPDATE NO ACTION;

ALTER TABLE "PlaylistTrack" ADD CONSTRAINT "FK_PlaylistTrackTrackId"
    FOREIGN KEY ("TrackId") REFERENCES "Track" ("TrackId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_PlaylistTrackTrackId" ON "PlaylistTrack" ("TrackId");

ALTER TABLE "Track" ADD CONSTRAINT "FK_TrackAlbumId"
    FOREIGN KEY ("AlbumId") REFERENCES "Album" ("AlbumId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_TrackAlbumId" ON "Track" ("AlbumId");

ALTER TABLE "Track" ADD CONSTRAINT "FK_TrackGenreId"
    FOREIGN KEY ("GenreId") REFERENCES "Genre" ("GenreId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_TrackGenreId" ON "Track" ("GenreId");

ALTER TABLE "Track" ADD CONSTRAINT "FK_TrackMediaTypeId"
    FOREIGN KEY ("MediaTypeId") REFERENCES "MediaType" ("MediaTypeId") ON DELETE NO ACTION ON UPDATE NO ACTION;

CREATE INDEX "IFK_TrackMediaTypeId" ON "Track" ("MediaTypeId");
