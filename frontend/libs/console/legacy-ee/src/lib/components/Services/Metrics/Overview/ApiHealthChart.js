import React from 'react';
import { Bar } from 'react-chartjs-2';
import moment from 'moment';
import { Tooltip } from '@hasura/console-legacy-ce';

import TopRequests from './TopRequests';
import styles from '../MetricsV1.module.scss';
import LoadingIcon from '../../../Common/LoadingIcon';
import LoaderCard from '../../../Common/LoaderCard';
import Placeholder from '../../../Placeholder/Placeholder';
import { FaInfoCircle } from 'react-icons/fa';

const formatters = {
  year: 'yyyy',
  month: "MMM 'yy",
  day: 'dd MMM',
  monthDayNHour: 'MMM DD H:mm',
  dayNHour: 'DD H:mm',
  hour: 'H:mm',
};

const getOptions = dataSize => {
  return {
    legend: {
      display: false,
      position: 'right',
    },
    scales: {
      xAxes: [
        {
          id: 'xAxis1',
          type: 'category',
          ticks: {
            fontSize: dataSize > 10 ? 9 : 11,
            display: true,
            callback: (val, index, values) => {
              if (index === 0) {
                return moment(val).format(formatters.monthDayNHour);
              }
              if (moment(val).isSame(moment(values[index - 1]), 'day')) {
                return moment(val).format(formatters.hour);
              }

              if (moment(val).isSame(moment(values[index - 1]), 'month')) {
                return moment(val).format(formatters.dayNHour);
              }

              return moment(val).format(formatters.monthDayNHour);
            },
          },
        },
      ],
      yAxes: [
        {
          id: 'y-axis-1',
          type: 'linear',
          display: true,
          position: 'left',
          ticks: {
            beginAtZero: true,
          },
        },
      ],
    },
  };
};

/**
 * apiHealthTooltip
 *
 * @param {*} messages array of messages, array separation means next line
 */

const apiHealthTooltip = ({ title = '', description = '' }) => (
  <div className={`${styles.flex} ${styles.pt_xs}`}>
    <span className={styles.left}>
      {title}
      <br />
      <br />

      <span className={styles.fontStyleMonoSpace}>{description}</span>
    </span>
  </div>
);
const ApiHealthChart = ({
  loading,
  header,
  value,
  valueUnit,
  color,
  datasource,
  highlightsQuery,
  footerHeader,
  roundOff = 2,
  projectId,
  dataKey,
  fromTime,
  yLabel,
  firstDataLoaded,
  tooltipMessage = {},
}) => {
  const { labels, data } = datasource.reduce(
    (a, c) => {
      const cY = Number(c.y);
      const y = Number.isNaN(cY) || cY < 0 ? 0 : cY;
      return {
        labels: [...a.labels, c.x],
        data: [...a.data, y],
      };
    },
    {
      labels: [],
      data: [],
    }
  );

  const state = {
    labels,
    datasets: [
      {
        label: yLabel,
        backgroundColor: color,
        data,
      },
    ],
  };
  return (
    <div>
      <p className={`${styles.strong} ${styles.sm} ${styles.fontWeightBold}`}>
        {header}
        <LoadingIcon loading={loading} />
      </p>
      <h3>
        {!loading ? (
          <span className={styles.strong}>
            {Number(value) ? Number(value).toFixed(roundOff) : value}
          </span>
        ) : (
          <Placeholder width={5} />
        )}
        <span className={styles.muted}>{valueUnit}</span>
        <Tooltip
          side="right"
          tooltipContentChildren={apiHealthTooltip(tooltipMessage)}
        >
          <FaInfoCircle className={styles.tooltipIcon} aria-hidden="true" />
        </Tooltip>
      </h3>
      <div className={styles.padding_10}>
        {!firstDataLoaded && loading ? (
          <LoaderCard />
        ) : (
          <Bar height={224} data={state} options={getOptions(data.length)} />
        )}
      </div>
      <TopRequests
        header={footerHeader}
        dataKey={dataKey}
        query={highlightsQuery}
        projectId={projectId}
        valueUnit={valueUnit}
        fromTime={fromTime}
      />
    </div>
  );
};

export default ApiHealthChart;
