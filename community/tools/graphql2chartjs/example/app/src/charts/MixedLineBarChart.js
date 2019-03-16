import React from 'react';
import { Bar } from 'react-chartjs-2';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import SyntaxHighlighter from 'react-syntax-highlighter';
import graphql2chartjs from 'graphql2chartjs';
import { Query } from 'react-apollo';
import gql from 'graphql-tag';

const query = `
  query {
    ArticleComments: article_stats {
      id
      label: title
      data: num_comments
    }
    ArticleLikes: article_stats {
      id
      label: title
      data: num_likes
    }
  }
`;

// Chart component
const Chart = () => (
  <Query
    query={gql`${query}`}
  >
    {
      ({data, error, loading}) => {
        if (loading || error) {
          console.error(error);
          return <div className="loadingIndicator">Please wait </div>;
        }
        // create graphql2chartjs instance
        const g2c = new graphql2chartjs();
        // add graphql data to graphql2chartjs instance while adding different chart types and properties
        g2c.add(data, (dataSetName, dataPoint) => {
          if (dataSetName === 'ArticleLikes') {
            // return bar chart properties for article likes
            return {
              ...dataPoint,
              chartType: 'bar',
              backgroundColor: '#44c0c1',
            }
          }
          // return line chart properties for article comments
          return {
            ...dataPoint,
            chartType: 'line',
            borderColor: '#ffce49',
            pointBackgroundColor: '#ffce49',
            backgroundColor: '#ffce49',
            fill: false
          }
        });
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

const MixedLineBarChart = ({ path }) => {
  return (
    <div style={{margin: '10px', paddingTop: '65px'}}>
      <div key="mixed">
        <div style={{marginBottom: '20px'}} id="mixed">
            <h2 style={{margin: '10px', textAlign: 'center'}}>Mixed chart (line and bar)</h2>
            <div className="chartWrapper">
              <div className="half_screen">
                <HighlightedQuery query={query} />
              </div>
              <div className="half_screen">
                <Chart />
              </div>
            </div>
          </div>
          <a href="https://github.com/hasura/graphql-engine/tree/master/community/tools/graphql2chartjs/example/app/src/charts/MixedLineBarChart.js">View source </a>
        <hr />
      </div>
    </div>
  )
}

export { MixedLineBarChart };
