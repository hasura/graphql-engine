import React from 'react';
import { Line } from 'react-chartjs-2';
import moment from 'moment';

export default function WebsocketOverTimeChart({
  data,
  format = 'DD MMM YYYY HH:mm',
}) {
  const labels = data.map(({ timestamp }) => moment(timestamp).format(format));
  const values = data.map(({ value }) => value);

  const chartData = {
    labels,
    datasets: [
      {
        label: 'Errors over time',
        backgroundColor: '#fff8ed',
        borderColor: '#fdb02c',
        borderWidth: 1,
        hoverBackgroundColor: '#fff8ed',
        hoverBorderColor: '#fdb02c',
        data: values,
      },
    ],
  };

  return (
    <div>
      <Line
        data={chartData}
        height={250}
        options={{
          maintainAspectRatio: false,
        }}
      />
    </div>
  );
}
