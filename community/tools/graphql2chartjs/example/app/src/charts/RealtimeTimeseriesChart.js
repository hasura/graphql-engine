import React, {useState, useEffect} from 'react';
import { Line } from 'react-chartjs-2';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import SyntaxHighlighter from 'react-syntax-highlighter';
import graphql2chartjs from 'graphql2chartjs';
import { ApolloConsumer } from 'react-apollo';
import gql from 'graphql-tag';

// Chart component
const Chart = ({client}) => {
  const [chartJsData, setChartjsData] = useState({});
  useEffect(
    () => {
        // initialize g2c
      const g2c = new graphql2chartjs();
      let latestTime = null;
      // load initial data
      client.query({
        query: gql`${firstQuery}`
      }).then((resp) => {
        // add the received data to g2c and update state
        g2c.add(resp.data, () => lineChartOptions);
        setChartjsData(g2c.data)

        // update the timestamp of the last received entry
        if (resp.data.StockPriceForAmazon.length) {
          latestTime = resp.data.StockPriceForAmazon[0].data_t;
        }

        // subscribe to a notification with newest data in the database
        client.subscribe({
          query: gql`${lastEventSubscription}`
        }).subscribe({
          next(event) {
            // if the data is not stale, fetch new data and add to g2c
            if (event.data.StockPriceForAmazon.length) {
              if (!latestTime || event.data.StockPriceForAmazon[0].data_t > latestTime) {
                fetchMore()
              }
            }
          },
          error(err) {
            console.error(err);
          }
        })
      })

      const fetchMore = () => {
        client.query({
          query: gql`${fetchMoreQuery}`,
          variables: {
            time: latestTime || "2019-03-12T19:16:45.640128+00:00"
          }
        }).then((resp) => {
          if (resp.data.StockPriceForAmazon.length) {
            g2c.add(resp.data, () => lineChartOptions);
            latestTime = resp.data.StockPriceForAmazon[0].data_t;
            setChartjsData(g2c.data);
          }
        })
      }
    },
    []
  )

  return (
    <Line
      data={chartJsData}
      options={{
        scales: {
          xAxes: [{
            type: 'time'
          }]
        },
        animation: {
          duration: 0, // general animation time
        },
        bezierCurve : false
      }}
    />
  )
}

/****************************************UTILS*****************************************/

const firstQuery = `
  query {
    StockPriceForAmazon: stocks (
      order_by: {
        created: desc
      }
      where: {
        ticker: {
          _eq: "AMZN"
        }
      }
      limit: 1000
    ) {
      data_t: created
      data_y: price
      ticker
    }
  }
`;

const lastEventSubscription = `
  subscription {
    StockPriceForAmazon: stocks (
      order_by: {
        created: desc
      }
      where: {
        ticker: {
          _eq: "AMZN"
        }
      }
      limit: 1
    ) {
      data_t: created
    }
  }
`

const fetchMoreQuery = `
  query ($time: timestamptz) {
    StockPriceForAmazon: stocks (
      order_by: {
        created: desc
      }
      where: {
        _and: [
          {
            ticker: {
              _eq: "AMZN"
            }
          },
          {
            created: {
              _gt: $time
            }
          }
        ]
      }
    ) {
      data_t: created
      data_y: price
      ticker
    }
  }
`

const HighlightedSubscription = () => (
  <SyntaxHighlighter
    language="graphql"
    style={docco}
  >
    {
      `
  # first query to load last 1000 data points${firstQuery}

  # subscription to detect any change in the database${lastEventSubscription}

  # fetch data newer than the locally existing data${fetchMoreQuery}
      `
    }
  </SyntaxHighlighter>
)

const RealtimeTimeseriesChart = ({ path }) => {
  return (
    <div style={{margin: '10px', paddingTop: '65px'}}>
      <div key="timeseries-chart">
        <div style={{marginBottom: '20px'}} id="timeseries-chart">
            <h2 style={{margin: '10px', textAlign: 'center'}}>Timeseries chart (with mock data)</h2>
            <div className="chartWrapper">
              <div className="half_screen">
                <HighlightedSubscription/>
              </div>
              <div className="half_screen">
                <ApolloConsumer>
                  {
                    client => <Chart client={client} />
                  }
                </ApolloConsumer>
              </div>
            </div>
          </div>
          <a href="something">View on codesandbox </a> { ' | ' } <a href="something">View source </a>
          <br/>
        <hr />
      </div>
    </div>
  )
}

const lineChartOptions = {
  chartType: 'line',
  fill: false,
  borderColor: 'brown',
  pointBackgroundColor: 'brown',
  showLine: false
}            

export { RealtimeTimeseriesChart  };
