import { readFileSync } from "node:fs";
import { createServer } from "node:http";
import { join } from "node:path";
import { createSchema, createYoga } from "graphql-yoga";
import { GraphQLError } from "graphql";
import DataLoader from "dataloader";
import { useDataLoader } from "@envelop/dataloader";
import { EnvelopArmorPlugin } from "@escape.tech/graphql-armor";
import type {
    Album,
    Artist,
    Genre,
    Resolvers,
    Track,
} from "./generated/graphql";
import sql from "./db";
import { genericBatchFunction } from "./dataloader";
import { keyByArray } from "./utils";

type Context = {
    getAlbumsById: DataLoader<string, Album>;
    getAllAlbums: DataLoader<string, Album[]>;
    getAlbumsByArtistId: DataLoader<string, Album[]>;
    getArtistsById: DataLoader<string, Artist>;
    getAllArtists: DataLoader<string, Artist[]>;
    getTracksById: DataLoader<string, Track>;
    getTracksByAlbumId: DataLoader<string, Track[]>;
    getAllTracks: DataLoader<string, Track[]>;
    getGenreById: DataLoader<string, Genre>;
    getAllGenres: DataLoader<string, Genre[]>;
    loggedInArtistId?: number;
};

const typeDefs = readFileSync(join(__dirname, "schema.graphql"), "utf8");

const resolvers: Resolvers = {
    Query: {
        Album_by_pk: async (_parent, args, context: Context, _info) => {
            return context.getAlbumsById.load(args.AlbumId.toString());
        },
        Album: async (_parent, _args, context: Context, _info) => {
            const albums = await context.getAllAlbums.load("1");
            for (const album of albums) {
                context.getAlbumsById.prime(album.AlbumId.toString(), album);
            }
            const albumsByArtistId = keyByArray(albums, "ArtistId");
            for (const [ArtistId, albums] of Object.entries(albumsByArtistId)) {
                context.getAlbumsByArtistId.prime(ArtistId, albums);
            }
            return albums;
        },
        Artist_by_pk: async (_parent, args, context: Context, _info) => {
            return context.getArtistsById.load(args.ArtistId.toString());
        },
        Artist: async (_parent, _args, context: Context, _info) => {
            if (context.loggedInArtistId) {
            }

            const artists = await context.getAllArtists.load("1");
            if (!artists) {
                throw new GraphQLError(`Albums not found.`);
            }
            for (const artist of artists) {
                context.getArtistsById.prime(artist.ArtistId.toString(), artist);
            }
            return artists;
        },
        Track_by_pk: async (_parent, args, context: Context, _info) => {
            return context.getTracksById.load(args.TrackId.toString());
        },
        Track: async (_parent, _args, context: Context, _info) => {
            const tracks = await context.getAllTracks.load("1");
            for (const track of tracks) {
                context.getTracksById.prime(track.TrackId.toString(), track);
            }
            const tracksByAlbumId = keyByArray(tracks, "AlbumId");
            for (const [AlbumId, tracks] of Object.entries(tracksByAlbumId)) {
                context.getTracksByAlbumId.prime(AlbumId, tracks);
            }
            return tracks;
        },
        Genre_by_pk: async (_parent, args, context: Context, _info) => {
            return context.getGenreById.load(args.GenreId.toString());
        },
        Genre: async (_parent, _args, context: Context, _info) => {
            const genres = await context.getAllGenres.load("1");
            if (!genres) {
                throw new GraphQLError(`Albums not found.`);
            }
            for (const genre of genres) {
                context.getGenreById.prime(genre.GenreId.toString(), genre);
            }
            return genres;
        },
    },
    Album: {
        async Artist(parent, _args, context: Context, _info) {
            return context.getArtistsById.load(parent.ArtistId.toString());
        },
        async Tracks(parent, _args, context: Context, _info) {
            const tracks = await context.getTracksByAlbumId.load(
                parent.AlbumId.toString()
            );
            for (const track of tracks) {
                context.getTracksById.prime(track.TrackId.toString(), track);
            }
            return tracks;
        },
    },
    Artist: {
        async Albums(parent, _args, context: Context, _info) {
            const albums = await context.getAlbumsByArtistId.load(
                parent.ArtistId.toString()
            );
            if (Array.isArray(albums)) {
                for (const album of albums) {
                    context.getAlbumsById.prime(album.AlbumId.toString(), album);
                }
            }
            return albums || [];
        },
    },
    Track: {
        async Album(parent, _args, context: Context, _info) {
            return context.getAlbumsById.load(parent.AlbumId!.toString());
        },
        async Genre(parent, _args, context: Context, _info) {
            return context.getGenreById.load(parent.GenreId!.toString());
        },
    },
};

export const schema = createSchema({
    typeDefs,
    resolvers,
});

const server = createServer(
    // @ts-ignore
    createYoga({
        graphqlEndpoint: "/v1/graphql",
        // @ts-ignore
        schema,
        plugins: [
            EnvelopArmorPlugin(),
            useDataLoader("getAlbumsById", (_context: Context) => {
                return new DataLoader((keys: Readonly<string[]>) =>
                    genericBatchFunction(keys, { name: "Album", id: "AlbumId" })
                );
            }),
            useDataLoader("getAllAlbums", (_context: Context) => {
                return new DataLoader(async (keys: Readonly<string[]>) => {
                    const albums = await sql`SELECT * FROM ${sql("Album")}`;
                    return keys.map((_key) => albums);
                });
            }),
            useDataLoader(
                "getAlbumsByArtistId",
                (_context: Context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Album", id: "ArtistId" }, true)
                    )
            ),
            useDataLoader(
                "getArtistsById",
                (_context: Context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Artist", id: "ArtistId" })
                    )
            ),
            useDataLoader("getAllArtists", (_context: Context) => {
                return new DataLoader(async (keys: Readonly<string[]>) => {
                    const artists = await sql`SELECT * FROM ${sql("Artist")}`;
                    return keys.map((_key) => artists);
                });
            }),
            useDataLoader(
                "getTracksById",
                (_context: Context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "TrackId" })
                    )
            ),
            useDataLoader(
                "getTracksByAlbumId",
                (_context: Context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "AlbumId" }, true)
                    )
            ),
            useDataLoader(
                "getAllTracks",
                (_context: Context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const tracks = await sql`SELECT * FROM ${sql("Track")}`;
                        return keys.map((_key) => tracks);
                    })
            ),
            useDataLoader(
                "getGenreById",
                (_context: Context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Genre", id: "GenreId" })
                    )
            ),
            useDataLoader(
                "getAllGenres",
                (_context: Context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const genres = await sql`SELECT * FROM ${sql("Genre")}`;
                        return keys.map((_key) => genres);
                    })
            ),
        ],
    })
);

server.listen(process.env["PORT"] || 8080, () => {
    console.info(
        `Server is running on http://localhost:${process.env["PORT"] || 8080
        }/v1/graphql`
    );
});
