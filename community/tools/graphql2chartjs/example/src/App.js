import React from 'react';
import './App.css';
import convert from 'graphql2chartjs';
import { Bar } from 'react-chartjs-2';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';

const App = ({ client }) => {

  return (
    <Subscription
      subscription={gql`
        subscription {
          VideoGameFollowers: video_games (
            order_by: {
              name: asc
            }
          ) {
            id
            label:name
            data: followers
            backgroundColor:color
          }
        }
      `}
    >
      {
        ({data, error, loading}) => {
          if (error) {
            console.error(error);
            return "Error";
          }
          if (loading) {
            return "Please wait..";
          }
          return (
            <Bar
              data={convert('bar', data)}
              width={600}
              height={250}
              options={{
                animation: {
                  duration: 0
                },
                legend: {
                  labels: {
                    boxWidth: 0
                  }
                }
              }}
            />
          )
        }
      }
    </Subscription>
  );
}

export default App;
