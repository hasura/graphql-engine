import React from 'react';
import { Line } from 'react-chartjs-2';
import '../Common/registerCharts';
import { format as formatDate } from 'date-fns';

export default function ErrorsOverTimeChart({
  data,
  format = 'dd MMM yyyy HH:mm',
}) {
  const items = data || [];
  const labels = items.map(({ timestamp }) =>
    formatDate(new Date(timestamp), format),
  );
  const values = items.map(({ value }) => value);

  const chartData = {
    labels,
    datasets: [
      {
        label: 'Errors over time',
        backgroundColor: '#fff8ed',
        borderColor: '#fdb02c',
        borderWidth: 1,
        fill: true,
        tension: 0.4,
        hoverBackgroundColor: '#fff8ed',
        hoverBorderColor: '#fdb02c',
        data: values,
      },
    ],
  };

  return (
    <div style={{ position: 'relative', height: 250 }}>
      <Line
        data={chartData}
        options={{
          maintainAspectRatio: false,
        }}
      />
    </div>
  );
}
