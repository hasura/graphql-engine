import React from 'react';
import { Line } from 'react-chartjs-2';
import moment from 'moment';

export default function ErrorsOverTimeChart({
  data,
  format = 'DD MMM YYYY HH:mm',
}) {
  const items = data || [];
  const labels = items.map(({ timestamp }) => moment(timestamp).format(format));
  const values = items.map(({ value }) => value);

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
