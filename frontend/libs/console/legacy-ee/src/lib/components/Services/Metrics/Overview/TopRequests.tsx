import React, { useEffect, useState } from 'react';
import { useQuery } from '@apollo/client/react';
import { Link } from 'react-router';
import styles from '../MetricsV1.module.scss';
import LoadingIcon from '../../../Common/LoadingIcon';
import { Flex, Skeleton } from '@radix-ui/themes';

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
  const fillerArray = new Array(lines).fill(0);
  return fillerArray.map((_, ix) => (
    <tr key={`key_${ix}`} className={styles.lineHeight_md}>
      <td className={styles.descriptionText}>
        <Skeleton width="30px" height="20px" />
      </td>

      <td className={styles.text_right}>
        <Skeleton width="30px" height="20px" />
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
    new Date().toISOString(),
  ]);
  const {
    loading,
    error,
    data: rawData,
  } = useQuery(query, {
    variables: {
      from_time: timeRange[0],
      to_time: timeRange[1],
      project_ids: `{${projectId}}`,
    },
  });
  useEffect(() => {
    setTimeRange([fromTime, new Date().toISOString()]);
  }, [fromTime]);

  const data = rawData as any;
  const filteredErrorRateList =
    dataKey === 'error_rate'
      ? data?.search_operation_name_summaries.filter(
          (errorObj) => errorObj.error_rate !== 0,
        )
      : [];
  const dataList =
    dataKey === 'error_rate'
      ? filteredErrorRateList
      : data?.search_operation_name_summaries;
  return (
    <div className={styles.topRequestWrapper}>
      <Flex className="font-bold" align="center" gap="2">
        {header}
        <LoadingIcon loading={loading} />
      </Flex>
      <div>
        {!loading &&
          dataList &&
          dataList.length > 0 &&
          dataList.map((value, key) => (
            <Flex key={key} align="center" justify="between">
              <div className={styles.descriptionText}>
                {value.operation_id ? (
                  <Link
                    to={`/pro/operations?filters=%5B%7B"value":%7B"start":"${timeRange[0]}","end":"${timeRange[1]}"%7D,"type":"time_range"%7D,%7B"type":"operation_id","value":"${value.operation_id}"%7D%5D`}
                  >
                    {value.operation_name || value.operation_id}
                  </Link>
                ) : (
                  '<Unknown Id>'
                )}
              </div>
              {!loading ? (
                <div className={styles.text_right}>
                  {parseValue(dataKey, value)}
                  {valueUnit}
                </div>
              ) : (
                <Skeleton width="14px" height="20px" />
              )}
            </Flex>
          ))}
        {loading && <Filler />}

        {!loading &&
          dataKey === 'error_rate' &&
          dataList?.length === 0 &&
          data?.search_operation_name_summaries?.length > 0 && (
            <div className={styles.descriptionText}>No errors</div>
          )}
        {!loading &&
          data?.search_operation_name_summaries &&
          Array.isArray(data?.search_operation_name_summaries) &&
          data?.search_operation_name_summaries?.length === 0 && (
            <div className={styles.descriptionText}>No results found!</div>
          )}
        {error && (
          <div className={styles.descriptionText}>Something went wrong!</div>
        )}
      </div>
    </div>
  );
};
export default React.memo(TopRequests);
