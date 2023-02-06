import { readFileSync } from "node:fs";
import { createServer } from "node:http";
import { join } from "node:path";
import { createSchema, createYoga } from "graphql-yoga";
import { GraphQLError } from "graphql";
import DataLoader from "dataloader";
import { useDataLoader } from "@envelop/dataloader";
import type { Album, Artist, Resolvers, Track } from "./generated/graphql";
import sql from "./db";
import { genericBatchFunction } from "./dataloader";
import { keyByArray } from "./utils";

const typeDefs = readFileSync(join(__dirname, "schema.graphql"), "utf8");

const resolvers: Resolvers = {
    Query: {
        Album: async (_parent, args, context, _info) => {
            return (context.getAlbumsById as DataLoader<string, Album>).load(args.id.toString());
        },
        Albums: async (_parent, _args, context, _info) => {
            const albums = await (
                context.getAllAlbums as DataLoader<string, Album[]>
            ).load('1');
            for (const album of albums) {
                (context.getAlbumsById as DataLoader<string, Album>).prime(
                    album.AlbumId.toString(),
                    album
                );
            }
            const albumsByArtistId = keyByArray(albums, 'ArtistId');
            for (const [ArtistId, albums] of Object.entries(albumsByArtistId)) {
                (context.getAlbumsByArtistId as DataLoader<string, Album[]>).prime(
                    ArtistId,
                    albums
                );
            }
            return albums;
        },
        Artist: async (_parent, args, context, _info) => {
            return (context.getArtistsById as DataLoader<string, Artist>).load(
                args.id.toString()
            );
        },
        Artists: async (_parent, _args, context, _info) => {
            const artists = await (
                context.getAllArtists as DataLoader<string, Artist[]>
            ).load('1');
            if (!artists) {
                throw new GraphQLError(`Albums not found.`);
            }
            for (const artist of artists) {
                (context.getArtistsById as DataLoader<string, Artist>).prime(
                    artist.ArtistId.toString(),
                    artist
                );
            }
            return artists;
        },
        Track: async (_parent, args, context, _info) => {
            return (context.getTracksById as DataLoader<string, Track>).load(args.id.toString());
        },
        Tracks: async (_parent, _args, context, _info) => {
            const tracks = await (
                context.getAllTracks as DataLoader<string, Track[]>
            ).load('1');
            for (const track of tracks) {
                (context.getTracksById as DataLoader<string, Track>).prime(
                    track.TrackId.toString(),
                    track
                );
            }
            const tracksByAlbumId = keyByArray(tracks, 'AlbumId');
            for (const [AlbumId, tracks] of Object.entries(tracksByAlbumId)) {
                (context.getTracksByAlbumId as DataLoader<string, Track[]>).prime(
                    AlbumId,
                    tracks
                );
            }
            return tracks;
        },
    },
    Album: {
        async Artist(parent, _args, context, _info) {
            return (context.getArtistsById as DataLoader<string, Artist>).load(
                parent.ArtistId.toString()
            );
        },
        async Tracks(parent, _args, context, _info) {
            const tracks = await (context.getTracksByAlbumId as DataLoader<string, Track[]>).load(
                parent.AlbumId.toString()
            );
            for (const track of tracks) {
                (context.getTracksById as DataLoader<string, Track>).prime(
                    track.TrackId.toString(),
                    track
                );
            }
            return tracks
        },
    },
    Artist: {
        async Albums(parent, _args, context, _info) {
            const albums = await (context.getAlbumsByArtistId as DataLoader<string, Album[]>).load(
                parent.ArtistId.toString()
            );
            if (Array.isArray(albums)) {
                for (const album of albums) {
                    (context.getAlbumsById as DataLoader<string, Album>).prime(
                        album.AlbumId.toString(),
                        album
                    );
                }

            }
            return albums || [];
        },
    },
    Track: {
        async Album(parent, _args, context, _info) {
            return (context.getAlbumsById as DataLoader<string, Album>).load(
                parent.AlbumId!.toString()
            );
        },
    }
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
            useDataLoader(
                "getAlbumsById",
                (_context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Album", id: "AlbumId" })
                    )
            ),
            useDataLoader(
                "getAllAlbums",
                (_context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const albums = await sql`SELECT * FROM ${sql("Album")}`;
                        return keys.map((_key) => albums);
                    })
            ),
            useDataLoader(
                "getAlbumsByArtistId",
                (_context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Album", id: "ArtistId" }, true)
                    )
            ),
            useDataLoader(
                "getArtistsById",
                (_context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Artist", id: "ArtistId" })
                    )
            ),
            useDataLoader(
                "getAllArtists",
                (_context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const artists = await sql`SELECT * FROM ${sql("Artist")}`;
                        return keys.map((_key) => artists);
                    })
            ),
            useDataLoader(
                "getTracksById",
                (_context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "TrackId" })
                    )
            ),
            useDataLoader(
                "getTracksByAlbumId",
                (_context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "AlbumId" }, true)
                    )
            ),
            useDataLoader(
                "getAllTracks",
                (_context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const tracks = await sql`SELECT * FROM ${sql("Track")}`;
                        return keys.map((_key) => tracks);
                    })
            ),
        ],
    })
);

server.listen(8080, () => {
    console.info("Server is running on http://localhost:8080/v1/graphql");
});
