import React from 'react';
import { Line } from 'react-chartjs-2';
import '../Common/registerCharts';
import { format as formatDate } from 'date-fns';
import { roundNumber } from '../../../../utils/math';

export default function UsageOverTimeChart({
  data,
  format = 'dd MMM yyyy HH:mm',
}) {
  const labels = data.map(({ timestamp }) =>
    formatDate(new Date(timestamp), format),
  );
  const values = data.map(({ success_count, error_count }) => {
    const total = success_count + error_count;
    const successRate = !total
      ? 100
      : roundNumber(success_count / total, 2) * 100;
    const errorRate = 100 - successRate;

    return {
      errorRate,
      successRate,
    };
  });
  const successRates = values.map(({ successRate }) => successRate);
  const errorRates = values.map(({ errorRate }) => errorRate);

  const chartData = {
    labels,
    datasets: [
      {
        label: '% error',
        backgroundColor: 'rgba(247,233,233, 0.5)',
        borderColor: '#e53935',
        borderWidth: 1,
        fill: true,
        tension: 0.4,
        hoverBackgroundColor: 'rgba(247,233,233, 0.5)',
        hoverBorderColor: '#e53935',
        data: errorRates,
      },
      {
        label: '% success',
        backgroundColor: 'rgba(240,248,231,0.5)',
        borderColor: '#69cb43',
        borderWidth: 1,
        fill: true,
        tension: 0.4,
        hoverBackgroundColor: 'rgba(240,248,231,0.5)',
        hoverBorderColor: '#69cb43',
        data: successRates,
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
