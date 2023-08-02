import React, { useEffect, useState } from 'react';
import moment from 'moment';
import { useQuery } from 'react-apollo';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';
import LoadingIcon from '../../../Common/LoadingIcon';
import Placeholder from '../../../Placeholder/Placeholder';

const parseValue = (dataKey, value) => {
  switch (dataKey) {
    case 'max_execution_time':
      return Number(value[dataKey] * 1000).toFixed(2);
    case 'error_rate':
      return Number(value[dataKey] * 100).toFixed(2);
    default:
      return Number(value[dataKey]).toFixed(2);
  }
};

const Filler = ({ lines = 5 }) => {
  const fillerArray = new Array(lines).fill();
  return fillerArray.map((_, ix) => (
    <tr key={`key_${ix}`} className={styles.lineHeight_md}>
      <td className={styles.descriptionText}>
        <Placeholder width={30} />
      </td>

      <td className={styles.text_right}>
        <Placeholder width={30} />
      </td>
    </tr>
  ));
};
const TopRequests = ({
  query,
  header,
  valueUnit,
  projectId,
  dataKey,
  fromTime,
}) => {
  const [timeRange, setTimeRange] = useState([
    fromTime,
    moment().toISOString(),
  ]);
  const { loading, error, data } = useQuery(query, {
    variables: {
      from_time: timeRange[0],
      to_time: timeRange[1],
      project_ids: `{${projectId}}`,
    },
  });
  useEffect(() => {
    setTimeRange([fromTime, moment().toISOString()]);
  }, [fromTime]);
  const filteredErrorRateList =
    dataKey === 'error_rate'
      ? data?.search_operation_name_summaries.filter(
          errorObj => errorObj.error_rate !== 0
        )
      : [];
  const dataList =
    dataKey === 'error_rate'
      ? filteredErrorRateList
      : data?.search_operation_name_summaries;
  return (
    <div className={styles.topRequestWrapper}>
      <div
        className={`${styles.strong} ${styles.mb_xs} ${styles.fontWeightBold}`}
      >
        {header}
        <LoadingIcon loading={loading} />
      </div>
      <table
        className={`${styles.pad_xs} ${styles.wd100Percent} ${styles.overflowAuto}`}
      >
        <tbody>
          {!loading &&
            dataList &&
            dataList.length > 0 &&
            dataList.map((value, key) => (
              <tr key={key} className={styles.lineHeight_md}>
                <td className={styles.descriptionText}>
                  {value.operation_id ? (
                    <Link
                      to={`/pro/operations?filters=%5B%7B"value":%7B"start":"${timeRange[0]}","end":"${timeRange[1]}"%7D,"type":"time_range"%7D,%7B"type":"operation_id","value":"${value.operation_id}"%7D%5D`}
                    >
                      {value.operation_name || value.operation_id}
                    </Link>
                  ) : (
                    '<Unknown Id>'
                  )}
                </td>
                {!loading ? (
                  <td className={styles.text_right}>
                    {parseValue(dataKey, value)}
                    {valueUnit}
                  </td>
                ) : (
                  <td className={styles.text_right}>
                    <Placeholder width={14} />
                  </td>
                )}
              </tr>
            ))}
          {loading && <Filler />}

          {!loading &&
            dataKey === 'error_rate' &&
            dataList?.length === 0 &&
            data?.search_operation_name_summaries?.length > 0 && (
              <tr>
                <td className={styles.descriptionText}>No errors</td>
              </tr>
            )}
          {!loading &&
            data?.search_operation_name_summaries &&
            Array.isArray(data?.search_operation_name_summaries) &&
            data?.search_operation_name_summaries?.length === 0 && (
              <tr>
                <td className={styles.descriptionText}>No results found!</td>
              </tr>
            )}
          {error && (
            <tr>
              <td className={styles.descriptionText}>Something went wrong!</td>
            </tr>
          )}
        </tbody>
      </table>
    </div>
  );
};
export default React.memo(TopRequests);
