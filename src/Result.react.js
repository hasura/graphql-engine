import React from 'react';
import {
  Subscription,
} from 'react-apollo';
import {Chart} from 'react-google-charts';
import gql from 'graphql-tag';

const SUBSCRIBE_RESULT = gql`
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
    ['Options'],
    ['Votes'],
  ];
  data.poll_results.map((r, i) => {
    d[0].push(r.option.text);
    d[1].push(r.votes);
  })

  return (
    <Chart
      width={'500px'}
      height={'300px'}
      chartType="Bar"
      loader={<div>Loading Chart</div>}
      data={d}
      options={{
        chart: {
          title: 'Poll results',
          subtitle: 'Realtime poll results',
        },
        animation: {
          duration: 1000,
          easing: 'out',
          startup: true,
        },
      }}
    />
  )
};

export const Result = (pollId) => (
  <Subscription subscription={SUBSCRIBE_RESULT} variables={pollId}>
    {({ loading, error, data }) => {
       if (loading) return <p>Loading...</p>;
       if (error) return <p>Error :</p>;
       return (
         <div>
           <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', maxWidth: 900 }}>
             {renderChart(data)}
           </div>
           <div>
             {
               data.poll_results.map((result, i) => (
                 <div key={i}>{`${result.option.text}: ${result.votes}`}</div>
               ))
             }
           </div>
         </div>
       );
    }}
  </Subscription>
)
