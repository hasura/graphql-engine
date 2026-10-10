import { Bar } from 'react-chartjs-2';
import type { ChartOptions, Scale } from 'chart.js';
import '../Common/registerCharts';
import { format, isSameDay, isSameMonth } from 'date-fns';
import TopRequests from './TopRequests';
import styles from '../MetricsV1.module.scss';
import LoadingIcon from '../../../Common/LoadingIcon';
import LoaderCard from '../../../Common/LoaderCard';
import { FaInfoCircle } from 'react-icons/fa';
import { Flex, Skeleton } from '@radix-ui/themes';
import { Tooltip } from '@hasura/shared/ui';

const formatters = {
  year: 'yyyy',
  month: "MMM ''yy",
  day: 'dd MMM',
  monthDayNHour: 'MMM dd H:mm',
  dayNHour: 'dd H:mm',
  hour: 'H:mm',
};

const getOptions = (dataSize: number): ChartOptions<'bar'> => {
  return {
    plugins: {
      legend: {
        display: false,
      },
    },
    scales: {
      x: {
        type: 'category',
        ticks: {
          font: { size: dataSize > 10 ? 9 : 11 },
          display: true,
          // Category ticks are passed their index; the label is the timestamp.
          callback(this: Scale, value, index, ticks) {
            const date = new Date(this.getLabelForValue(Number(value)));
            if (index === 0) {
              return format(date, formatters.monthDayNHour);
            }
            const prevDate = new Date(
              this.getLabelForValue(Number(ticks[index - 1].value)),
            );
            if (isSameDay(date, prevDate)) {
              return format(date, formatters.hour);
            }

            if (isSameMonth(date, prevDate)) {
              return format(date, formatters.dayNHour);
            }

            return format(date, formatters.monthDayNHour);
          },
        },
      },
      y: {
        type: 'linear',
        display: true,
        position: 'left',
        beginAtZero: true,
      },
    },
  };
};

/**
 * apiHealthTooltip
 *
 * @param {*} messages array of messages, array separation means next line
 */

const apiHealthTooltip = ({ title = '', description = '' }) => (
  <div className={`${styles['flex']} ${styles['pt_xs']}`}>
    <span className={styles['left']}>
      {title}
      <br />
      <br />

      <span className={styles['fontStyleMonoSpace']}>{description}</span>
    </span>
  </div>
);
const ApiHealthChart = ({
  loading = false,
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
    },
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
    <div className="w-full">
      <p className="font-bold">
        {header} <LoadingIcon loading={loading} />
      </p>
      <Skeleton loading={loading}>
        <Flex align="center" gap="1">
          <span className="font-bold">
            {Number(value) ? Number(value).toFixed(roundOff) : value}
          </span>
          <span>{valueUnit}</span>
          <Tooltip side="right" content={apiHealthTooltip(tooltipMessage)}>
            <FaInfoCircle
              className={styles['tooltipIcon']}
              aria-hidden="true"
              color="#505050"
            />
          </Tooltip>
        </Flex>
      </Skeleton>
      <div className="my-4">
        {!firstDataLoaded && loading ? (
          <LoaderCard />
        ) : (
          // 300px was react-chartjs-2 v2's default width; together with the
          // height it fixes the aspect ratio the chart had before.
          <Bar
            width={420}
            height={320}
            data={state}
            options={getOptions(data.length)}
          />
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
