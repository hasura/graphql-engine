import React from 'react';
import './App.css';
import graphql2chartjs from 'graphql2chartjs';
import { Line } from 'react-chartjs-2';
import { Subscription} from 'react-apollo';
import gql from 'graphql-tag';

const App = ({ client }) => {

  return (
    <Subscription
      subscription={gql`
        subscription {
          AmazonValuation: stocks(where: {ticker: {_eq: "AMZN"}}, order_by: {created: desc}, limit: 100) {
            data_y: price
            data_t: created
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
          const g2c = new graphql2chartjs(data, 'line');
          return (
            <Line
              data={g2c.data}
              options={{
                scales: {
                  xAxes: [{
                    type: 'time'
                  }]
                },
                bezierCurve : false
              }}
              height={100}
            />
          )
        }
      }
    </Subscription>
  );
}

export default App;
