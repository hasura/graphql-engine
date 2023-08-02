import React, { useEffect, useRef } from 'react';
import { useQuery } from 'react-apollo';
import { Col, Row } from 'react-bootstrap';
import { Tooltip } from '@hasura/console-legacy-ce';
import ApiHealthChart from './ApiHealthChart';
import styles from '../MetricsV1.module.scss';
import {
  fetchApiHealthData,
  fetchTop5Requests,
  fetchTop5ErrorRates,
  fetchTop5Latencies,
} from './graphql.queries';
import WebSocketInfo from './WebSocketInfo';
import { addNumbersSafely } from '../../../../utils/validation';
import { getTimeIntervalFromRange } from '../utils';
import Header from './Header';
import { FaInfoCircle } from 'react-icons/fa';

const roundOff = 2;
let chartHeight;

const parseChartData = (datasource, x, y) =>
  datasource?.map(i => ({ x: i[x], y: Number(i[y]).toFixed(roundOff) }));

const parseErrorRate = datasource =>
  datasource?.map(i => {
    const diff = i?.request_count - i?.success_count || 0;
    const average = Number((diff / i.request_count) * 100).toFixed(roundOff);
    const y = Number.isNaN(average) ? 0 : average;
    return {
      x: i.bucket_time,
      y,
    };
  });

const getAverageRPM = (data = []) => {
  let requestsSUM = 0;
  let totalbucketsWithData = 0;
  data.forEach(c => {
    if ('requests_per_minute' in c && c.requests_per_minute !== null) {
      requestsSUM = addNumbersSafely(requestsSUM, c.requests_per_minute);
      totalbucketsWithData++;
    }
  });
  if (requestsSUM && totalbucketsWithData) {
    return requestsSUM / totalbucketsWithData;
  }
  return 0;
};

const getAverageErrorRate = (data = []) => {
  const total = data.reduce(
    (a, c) => {
      return {
        success_count: addNumbersSafely(a.success_count, c.success_count),
        request_count: addNumbersSafely(a.request_count, c.request_count),
      };
    },
    {
      success_count: 0,
      request_count: 0,
    }
  );
  const response =
    ((total.request_count - total.success_count) / total.request_count) * 100;
  if (Number.isNaN(response)) return 0;
  return response;
};
const getAverageExecutionRate = (data = []) => {
  let requestsSUM = 0;
  let totalbucketsWithData = 0;
  data.forEach(c => {
    if ('average_execution_time' in c && c.average_execution_time !== null) {
      requestsSUM = addNumbersSafely(requestsSUM + c.average_execution_time);
      totalbucketsWithData++;
    }
  });
  if (requestsSUM && totalbucketsWithData) {
    return requestsSUM / totalbucketsWithData;
  }
  return 0;
};
const getBucketSize = (fromTime, toTime) => {
  const diff = new Date(toTime) - new Date(fromTime);
  // less than 2 hours
  // 1 second buffer is added
  if (diff <= 1000 * 60 * 60 * 2 + 1000) {
    return 1000 * 60 * 5; // 5 mins
  }
  if (diff <= 1000 * 60 * 60 * 24 + 1000) {
    return 1000 * 60 * 60; // 1 hr
  }
  return 1000 * 60 * 60 * 24; // 1 day
};
const gapFillData = (data, fromTime, toTime) => {
  const bucketSize = getBucketSize(fromTime, toTime);
  const result = [];
  let dataPointer = 0;

  for (
    let timePointer =
      new Date(fromTime).getTime() -
      (new Date(fromTime).getTime() % bucketSize);
    timePointer < new Date(toTime).getTime();
    timePointer += bucketSize
  ) {
    if (data[dataPointer] && data[dataPointer]?.bucket_time) {
      const nextItemFromServer = new Date(
        data[dataPointer]?.bucket_time
      ).getTime();
      if (
        nextItemFromServer >= timePointer &&
        nextItemFromServer < timePointer + bucketSize
      ) {
        result.push({
          ...data[dataPointer],
          average_execution_time:
            data[dataPointer]?.average_execution_time * 1000,
        });
        dataPointer++;
      } else {
        result.push({
          bucket_time: new Date(timePointer),
          requests_per_minute: null,
          average_execution_time: null,
        });
      }
    } else {
      result.push({
        bucket_time: new Date(timePointer),
        requests_per_minute: null,
        average_execution_time: null,
      });
    }
  }

  return result;
};

const ApiHealth = ({
  projectId,
  liveStats,
  fromTime,
  toTime = new Date(),
  setFromTime = () => {},
}) => {
  const { loading, error, data, refetch } = useQuery(fetchApiHealthData, {
    variables: {
      args: {
        project_ids: `{${projectId}}`,
        from_time: fromTime,
        time_interval: getTimeIntervalFromRange(fromTime),
      },
    },
  });
  const firstDataLoaded = useRef();
  useEffect(() => {
    refetch();
  }, [fromTime]);
  useEffect(() => {
    if (data?.search_general_operation_metrics_over_time) {
      firstDataLoaded.current = true;
    }
  }, [data]);
  const chartDiv = useRef();

  useEffect(() => {
    setTimeout(() => {
      if (chartDiv.current) {
        chartHeight = chartDiv?.current?.offsetHeight + 5;
      }
    }, 500); // time to fade in
  }, [chartDiv, data, fromTime]);

  if (error) console.error(error);

  const dataSource = gapFillData(
    data?.search_general_operation_metrics_over_time || [],
    fromTime,
    toTime
  );

  // chart datasources
  const error_rate_chart_data = data ? parseErrorRate(dataSource) : [];
  const request_rate_chart_data = data
    ? parseChartData(dataSource, 'bucket_time', 'requests_per_minute')
    : [];
  const average_execution_time_data = data
    ? parseChartData(dataSource, 'bucket_time', 'average_execution_time')
    : [];
  const apiHealthTooltip = (
    <div className={`${styles.flex} ${styles.pt_xs}`}>
      <span className={styles.left}>
        Each graph interval is averaged over the following time intervals:
        <br />
        <br />
        If date range is &lt;= 1 hour then every 5 minutes
        <br />
        If date range is &gt;= 8 hours then every hour
      </span>
    </div>
  );
  return (
    <>
      <Row className={styles.pad_sm_sides}>
        <div className={`${styles.display_flex} ${styles.pb_sm}`}>
          <p className={`${styles.strong} ${styles.mr_xs}`}>API Metrics</p>
          <p className={`${styles.muted}`}>
            (Averages)
            <span>
              <Tooltip side="right" tooltipContentChildren={apiHealthTooltip}>
                <FaInfoCircle
                  className={styles.tooltipIcon}
                  aria-hidden="true"
                />
              </Tooltip>
            </span>
          </p>
          <Header fromTime={fromTime} setFromTime={setFromTime} />
          <WebSocketInfo liveStats={liveStats} />
        </div>
      </Row>
      {error && (
        <h3 style={{ ...(chartHeight && { minHeight: chartHeight }) }}>
          Error fetching API Metrics Data!
        </h3>
      )}
      {!error && (
        <div
          className={`${styles.animated} ${styles.row} ${styles.fadeIn} ${styles.inline} ${styles.wd100Percent} bootstrap-jail`}
          ref={chartDiv}
        >
          <Col md={4}>
            <ApiHealthChart
              loading={loading}
              header="Request Rate"
              yLabel="Request Rate (rpm)"
              value={getAverageRPM(dataSource)}
              valueUnit="rpm"
              color="#003f5c"
              datasource={request_rate_chart_data}
              highlightsQuery={fetchTop5Requests}
              dataKey="requests_per_minute"
              footerHeader="Most Request (RPM)"
              projectId={projectId}
              fromTime={fromTime}
              firstDataLoaded={firstDataLoaded?.current}
              tooltipMessage={{
                title: 'Average RPM is calculated as:',
                description:
                  'total requests during the time range / time range in minutes',
              }}
            />
          </Col>
          <Col md={4}>
            <ApiHealthChart
              loading={loading}
              header="Error Rate"
              yLabel="Error Rate (%)"
              value={getAverageErrorRate(dataSource)}
              valueUnit="%"
              color="rgb(122, 81, 149)"
              highlightsQuery={fetchTop5ErrorRates}
              dataKey="error_rate"
              datasource={error_rate_chart_data}
              footerHeader="Highest Error Rate (%)"
              projectId={projectId}
              fromTime={fromTime}
              firstDataLoaded={firstDataLoaded?.current}
              tooltipMessage={{
                title: 'Average Error Rate is calculated as:',
                description:
                  '(total errors during the time range / time range in minutes) × 100',
              }}
            />
          </Col>
          <Col md={4}>
            <ApiHealthChart
              loading={loading}
              header="Avg. Execution Time"
              yLabel="Avg. Execution Time (ms)"
              value={getAverageExecutionRate(dataSource)}
              valueUnit="ms"
              color="#ef5675"
              roundOff={2}
              datasource={average_execution_time_data}
              projectId={projectId}
              highlightsQuery={fetchTop5Latencies}
              dataKey="max_execution_time"
              fromTime={fromTime}
              firstDataLoaded={firstDataLoaded?.current}
              tooltipMessage={{
                title: 'Average execution time is calculated as:',
                description:
                  'total time taken by all requests during time range / time range in minutes',
              }}
              footerHeader="Highest Latency (ms)"
            />
          </Col>
        </div>
      )}
    </>
  );
};
export default ApiHealth;
