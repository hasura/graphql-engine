import React, { useState } from "react"
import { ApolloProvider } from 'react-apollo';
import {Query} from 'react-apollo';
import gql from 'graphql-tag';
import './style.css';

const query = gql`
	query PlaylistQuery {
	  playlist {
	    name
	    tracks {
	      track_details {
	        name
	      }
	      track {
	        items {
	          track {
	            url
	          }
	        }
	      }
	    }
	  }
	}
`;

const mutation = gql`
	mutation insert_playlist($name: String!) {
	  insert_playlist(objects: [{
	    name: $name
	  }]) {
	    affected_rows
	  }
	}
`;


const Playlist = ({ client }) => {
  const [showNewPlaylist, updateShowNewPlaylist] = useState(false);
  const [playlistName, setPlaylistName] = useState('');
  const createPlaylist = (playlistName) => {
  	client.mutate({
      mutation: mutation,
      variables: {name: playlistName}
    }).then((data) => {
    	//TODO: update cache
    })
  }
  return (
    <ApolloProvider client={client}>
	    <Query query={query}>
	      {({ loading, error, data, client}) => {
	        if (loading) {
	          return (<div>Loading...</div>);
	        }
	        /*
	        if (error) {
	          console.error(error);
	          return (<div>Error!</div>);
	        }
	        */
	        return (
	        	<div>
			    	<div className={'newPlaylist'}>
			    		<button
			    			className={'playlistBtn'}
			    			onClick={() => updateShowNewPlaylist(true)}
			    		>+ New Playlist</button>
			    		{showNewPlaylist ? 
			    			(
			    			  <div>
			    			  Playlist name:
			    			  <input type="text" onChange={(e) => setPlaylistName(e.target.value)} />
			    			  <button onClick={() => createPlaylist(playlistName)}>Create</button>
			    			  </div>
			    			)
			    			:
			    			null
			    		}
				    </div>
		        	<h2>My Playlist</h2>
		        	{data && data.playlist.length ? null : <div>No playlists available</div>}
		        	{data && data.playlist.map((p, i) => (
		        		<div key={i+p.name}>
		        		<b>{p.name}</b>
		        		{p.tracks.map((t,j) => {
		        			let elem = null;
		        			if (t.track) {
		        				elem = (
		        					<div key={t+j}>
		        					<div>{t.track_details.name}</div>
		        					<div>
		        					{t.track.items[0] ? (
		        						<audio controls>
		        						<source src={t.track.items[0].track.url} type="audio/mp3" />
		        						Your browser does not support the audio element.
		        						</audio>
		        						) : null}
		        					</div>
		        					</div>
		        					)
		        			}
		        			return elem;
		        		})}
		        		</div>
		        		))}
	        	</div>

	        );
	      }}
	    </Query>
	</ApolloProvider>
  )
};

export default Playlist;
