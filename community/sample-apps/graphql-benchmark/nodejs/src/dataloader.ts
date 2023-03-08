import sql from "./db";
import type { Album, Artist, Track } from "./generated/graphql";
import { keyByArray } from "./utils";

export async function genericBatchFunction<T extends Album | Artist | Track>(
    keys: Readonly<number[]> | Readonly<string[]>,
    { name, id }:
        | { name: "Album"; id: "AlbumId" }
        | { name: "Album"; id: "ArtistId" }
        | { name: "Artist"; id: "ArtistId" }
        | { name: "Track"; id: "TrackId" }
        | { name: "Track"; id: "AlbumId" }
        | { name: "Genre"; id: "GenreId" },
    resultsAreArray = false,
) {
    // console.log(name, id, keys)
    const results = await sql<T[]>`SELECT * FROM ${sql(
        name
    )} WHERE ${sql(id)} in ${sql(keys)}`;

    const resultsMap = keyByArray<T>(results, id as any);

    return keys.map((key: typeof keys[number]) => resultsAreArray ? resultsMap[key] : resultsMap[key]![0]);
}
