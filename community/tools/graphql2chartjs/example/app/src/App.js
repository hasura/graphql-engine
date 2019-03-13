import React from 'react';
import './App.css';
import graphql2chartjs from 'graphql2chartjs';
import { Bar } from 'react-chartjs-2';
import { Query} from 'react-apollo';
import gql from 'graphql-tag';

const App = () => {

  return (
    <Query
      query={gql`
        query {
          artist_albums {
            label: artist {
              name
            }
            data: count
          }
        }
      `}>
      {
        ({data, error, loading}) => {
          if (error) {
            console.error(error);
            return "Error";
          }
          if (loading) {
            return "Please wait..";
          }
          const g2c = new graphql2chartjs(data, (dataset, record) => {
            return {
              chartType: 'bar',
              label: record.label.name
            };
          });
          return (
            <Bar data={g2c.data} />
          )
        }
      }
    </Query>
  );
}

export default App;
