import React from 'react';
import { Bar } from 'react-chartjs-2';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import SyntaxHighlighter from 'react-syntax-highlighter';
import graphql2chartjs from 'graphql2chartjs';
import { Query } from 'react-apollo';
import gql from 'graphql-tag';

const query = `
  query {
    ArticleLikes: article_stats {
      id
      label: title
      data: num_likes
    }
  }
`;

// Chart component
const Chart = ({ query }) => (
  <Query
    query={gql`${query}`}
  >
    {
      ({data, error, loading}) => {
        if (loading || error) {
          return <div className="loadingIndicator">Please wait </div>;
        }
        // create graphql2chartjs instance
        const g2c = new graphql2chartjs();
        // add graphql data to graphql2chartjs instance
        g2c.add(data, 'bar');
        // render chart with g2c data :)
        return (
          <Bar data={g2c.data} />
        )
      }
    }
  </Query>
)

/****************************************UTILS*****************************************/

const HighlightedQuery = ({ query }) => (
  <SyntaxHighlighter
    language="graphql"
    style={docco}
  >
    {query}
  </SyntaxHighlighter>
)

const BasicBarChart = ({ path }) => {
  return (
    <div style={{margin: '10px', paddingTop: '65px'}}>
      <div key="bar">
        <div style={{marginBottom: '20px'}} id="bar">
            <h2 style={{margin: '10px', textAlign: 'center'}}>Basic bar chart</h2>
            <div className="chartWrapper">
              <div className="half_screen">
                <HighlightedQuery query={query} />
              </div>
              <div className="half_screen">
                <Chart query={query}/>
              </div>
            </div>
          </div>
          <a href="https://github.com/hasura/graphql-engine/tree/master/community/tools/graphql2chartjs/example/app/src/charts/BasicBarChart.js">View source </a>
        <hr />
      </div>
    </div>
  )
}

export { BasicBarChart };
