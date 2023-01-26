import { readFileSync } from "node:fs";
import { createServer } from "node:http";
import { createSchema, createYoga } from "graphql-yoga";
import { GraphQLError } from "graphql";
import DataLoader from "dataloader";
import { useDataLoader } from "@envelop/dataloader";
import type { Album, Artist, Resolvers, Track } from "./generated/graphql";
import sql from "./db";
import { genericBatchFunction } from "./dataloader";
import { keyByArray } from "./utils";

const typeDefs = readFileSync("../schema.graphql", "utf8");

const resolvers: Resolvers = {
    Query: {
        Album: async (parent, args, context, info) => {
            return (context.getAlbumsById as DataLoader<string, Album>).load(args.id.toString());
        },
        Albums: async (parent, args, context, info) => {
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
        Artist: async (parent, args, context, info) => {
            return (context.getArtistsById as DataLoader<string, Artist>).load(
                args.id.toString()
            );
        },
        Artists: async (parent, args, context, info) => {
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
        Track: async (parent, args, context, info) => {
            return (context.getTracksById as DataLoader<string, Track>).load(args.id.toString());
        },
        Tracks: async (parent, args, context, info) => {
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
        async Artist(parent, args, context, info) {
            return (context.getArtistsById as DataLoader<string, Artist>).load(
                parent.ArtistId.toString()
            );
        },
        async Tracks(parent, args, context, info) {
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
        async Albums(parent, args, context, info) {
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
        async Album(parent, args, context, info) {
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
    createYoga({
        schema,
        plugins: [
            useDataLoader(
                "getAlbumsById",
                (context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Album", id: "AlbumId" })
                    )
            ),
            useDataLoader(
                "getAllAlbums",
                (context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const albums = await sql`SELECT * FROM ${sql("Album")}`;
                        return keys.map((key) => albums);
                    })
            ),
            useDataLoader(
                "getAlbumsByArtistId",
                (context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Album", id: "ArtistId" }, true)
                    )
            ),
            useDataLoader(
                "getArtistsById",
                (context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Artist", id: "ArtistId" })
                    )
            ),
            useDataLoader(
                "getAllArtists",
                (context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const artists = await sql`SELECT * FROM ${sql("Artist")}`;
                        return keys.map((key) => artists);
                    })
            ),
            useDataLoader(
                "getTracksById",
                (context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "TrackId" })
                    )
            ),
            useDataLoader(
                "getTracksByAlbumId",
                (context) =>
                    new DataLoader((keys: Readonly<string[]>) =>
                        genericBatchFunction(keys, { name: "Track", id: "AlbumId" }, true)
                    )
            ),
            useDataLoader(
                "getAllTracks",
                (context) =>
                    new DataLoader(async (keys: Readonly<string[]>) => {
                        const tracks = await sql`SELECT * FROM ${sql("Track")}`;
                        return keys.map((key) => tracks);
                    })
            ),
        ],
    })
);

server.listen(4000, () => {
    console.info("Server is running on http://localhost:4000/graphql");
});
