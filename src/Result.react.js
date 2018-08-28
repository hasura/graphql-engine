import React from 'react';
import {
  Subscription,
} from 'react-apollo';
import {
  Well,
} from 'react-bootstrap';
import {Chart} from 'react-google-charts';
import gql from 'graphql-tag';

const SUBSCRIBE_RESULT = `
  subscription getResult($pollId: uuid!) {
    poll_results (
      where: {
        poll_id: {_eq: $pollId}
      }
    ) {
      option {
        id
        text
      }
      votes
    }
  }
`;

const renderChart = (data) => {
  const d = [
    ['Option', 'No. of votes', {role:'annotation'}, {role:'style'}],
  ];
  data.poll_results.map((r, i) => {
    d.push([r.option.text, r.votes, r.votes, 'color: #4285f4']);
  })

  return (
    <Chart
      width={'700px'}
      height={'400px'}
      chartType="BarChart"
      loader={<div>Loading Chart</div>}
      data={d}
      options={{
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
  <Subscription subscription={gql`${SUBSCRIBE_RESULT}`} variables={pollId}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div>
           <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', maxWidth: 900 }}>
             {renderChart(data)}
           </div>
           <div><pre>{SUBSCRIBE_RESULT}</pre></div>
           <div>
             {/*
               data.poll_results.map((result, i) => (
                 <div key={i}>{`${result.option.text}: ${result.votes}`}</div>
               ))
             */}
           </div>
         </div>
       );
    }}
  </Subscription>
)
