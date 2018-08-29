import React from 'react';
import {
  Subscription,
} from 'react-apollo';
import {
  Well,
} from 'react-bootstrap';
import {Chart} from 'react-google-charts';
import gql from 'graphql-tag';
import { SUBSCRIPTION_RESULT } from './GraphQL.jsx'

const renderChart = (data) => {
  const d = [
    ['Option', 'No. of votes', {role:'annotation'}, {role:'style'}],
  ];
  data.poll_results.map((r, i) => {
    d.push([r.option.text, r.votes, r.votes, 'color: #4285f4']);
  })

  return (
    <Chart className="poll-result-chart-container"
      chartType="BarChart"
      loader={<div>Loading Chart</div>}
      data={d}
      options={{
        height: '100%',
        chart: {
          title: 'Realtime results',
        },
        legend: { position: 'none' },
        animation:{
          duration: 1000,
          easing: 'out',
          startup: true,
        },
      }}
    />
  )
};

export const Result = (pollId) => (
  <Subscription subscription={gql`${SUBSCRIPTION_RESULT}`} variables={pollId}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div>
           <div>
             {renderChart(data)}
           </div>
         </div>
       );
    }}
  </Subscription>
)
