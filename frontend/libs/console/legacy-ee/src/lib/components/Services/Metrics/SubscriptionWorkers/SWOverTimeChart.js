import React from 'react';
import { Line } from 'react-chartjs-2';
import moment from 'moment';
import { roundToTwo } from './utils';

import styles from '../Metrics.module.scss';

export default function SWOverTimeChart({
  executionTimeData,
  postgresData,
  subscribersData,
  format = 'DD MMM YYYY HH:mm',
}) {
  const executionTimelabels = executionTimeData.map(({ timestamp }) =>
    moment(timestamp).format(format)
  );
  const postgreslabels = postgresData.map(({ timestamp }) =>
    moment(timestamp).format(format)
  );
  const subscriberslabels = subscribersData.map(({ timestamp }) =>
    moment(timestamp).format(format)
  );

  const executionTimes = executionTimeData.map(({ value }) =>
    roundToTwo(value * 1000)
  );
  const postgresQueries = postgresData.map(({ value }) => value);
  const subscribers = subscribersData.map(({ value }) => value);

  const getMaxMin = values => {
    const max = Math.max(...values);
    const min = Math.min(...values);
    const diff = max - min;
    if (diff >= 6) {
      return { max: max, min: min };
    } else if (diff < 6 && min - 3 < 0) {
      return { max: max + (6 - diff), min: min };
    }
    return { max: max + 3, min: min - 3 };
  };

  const postgresQueryValues = getMaxMin(postgresQueries);
  const subscribersValues = getMaxMin(subscribers);

  const executionTimeChartData = {
    labels: executionTimelabels,
    datasets: [
      {
        label: 'Average execution time over time',
        backgroundColor: '#fff8ed',
        borderColor: '#fdb02c',
        borderWidth: 1,
        data: executionTimes,
      },
    ],
  };

  const postgresChartData = {
    labels: postgreslabels,
    datasets: [
      {
        label: 'Total number of Postgres queries over time',
        backgroundColor: '#fff8ed',
        borderColor: '#fdb02c',
        borderWidth: 1,
        data: postgresQueries,
      },
    ],
  };

  const subscribersChartData = {
    labels: subscriberslabels,
    datasets: [
      {
        label: 'Total number of subscribers over time',
        backgroundColor: '#fff8ed',
        borderColor: '#fdb02c',
        borderWidth: 1,
        data: subscribers,
      },
    ],
  };

  return (
    <div className={styles.flexRow}>
      <div>
        <Line
          data={executionTimeChartData}
          height={250}
          width={378}
          options={{
            maintainAspectRatio: false,
          }}
        />
      </div>
      <div>
        <Line
          data={postgresChartData}
          height={250}
          width={378}
          options={{
            maintainAspectRatio: false,
            scales: {
              yAxes: [
                {
                  ticks: {
                    max: postgresQueryValues.max,
                    min: postgresQueryValues.min,
                  },
                },
              ],
            },
          }}
        />
      </div>
      <div>
        <Line
          data={subscribersChartData}
          height={250}
          width={378}
          options={{
            maintainAspectRatio: false,
            scales: {
              yAxes: [
                {
                  ticks: {
                    max: subscribersValues.max,
                    min: subscribersValues.min,
                  },
                },
              ],
            },
          }}
        />
      </div>
    </div>
  );
}
