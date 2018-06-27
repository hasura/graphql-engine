var query = `
query albums_tracks_genre_all {
  albums {
    id
    title
    tracks {
      id
      name
      genre {
        name
      }
    }
  }
}

query albums_tracks_genre_some {
  albums (where: {artist_id: {_eq: 127}}){
    id
    title
    tracks {
      id
      name
      genre {
        name
      }
    }
  }
}

query tracks_media_all {
  tracks {
    id
    name
    media_type {
      name
    }
  }
}

query tracks_media_some {
  tracks (where: {composer: {_eq: "Kurt Cobain"}}){
    id
    name
    album {
      id
      title
    }
    media_type {
      name
    }
  }
}

query artists_collaboration {
  artists(where: {
  	  albums: {
        tracks: {
          composer: {_eq: "Ludwig van Beethoven"}
        }
      }
    })
  {
    id
    name
  }
}

query artistByArtistId {
  artists(where: {id: {_eq: 3}}) {
    id
    name
  }
}
`;
var variables=`
{
}
`;
exports.query = query;
exports.variables = variables;
