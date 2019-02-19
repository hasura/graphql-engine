import React, { Component } from 'react';
import './App.css';
import convert from 'graphql2chartjs';
import { Bar } from 'react-chartjs-2';
import { Subscription } from 'react-apollo';
import gql from 'graphql-tag';

class App extends Component {
  render() {
    return (
      <Subscription
        subscription={gql`
          subscription {
            Followers: video_games (
              order_by: {
                name: asc
              }
            ) {
              label: name
              backgroundColor: color
              data: followers
            }
          }
        `}
      >
        {
          ({data, error, loading}) => {
            console.log(data,error,loading);
            if (error || loading ) {
              return "Please wait..";
            }
            const chartJsData = convert('bar', data);
            return (
              <Bar
                data={chartJsData}
                width={600}
                height={250}
              />
            )
          }
        }
      </Subscription>
    );
  }
}

export default App;
