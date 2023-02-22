import ToolTip from '../../../Common/Tooltip/Tooltip';
import { isNumber, isFloat } from '../../../Common/utils/jsUtils';
import { TaskEvent } from '../../../../features/ConnectDB';
import { Badge } from '../../../../new-components/Badge';
import React, { useState } from 'react';
import {
  FaCheck,
  FaChevronDown,
  FaChevronUp,
  FaExclamationTriangle,
  FaMinusCircle,
} from 'react-icons/fa';

import { driverToLabel } from '../../../../dataSources';
import { DataSource } from '../../../../metadata/types';

type LatencyData = Exclude<
  TaskEvent['public_event_data']['sources'],
  null
>[number];

type CollapsibleToggleProps = {
  dataSource: DataSource;
  dbVersion: string;
  dbLatencyData?: LatencyData;
};

const getBadgeColorForLatencyData = (latencyData: LatencyData) => {
  if (latencyData.avg_latency <= 100) {
    return 'green';
  }

  if (latencyData.avg_latency > 100 && latencyData.avg_latency <= 200) {
    return 'yellow';
  }

  return 'red';
};

const getBadgeTextForLatencyData = (latencyData: LatencyData) => {
  if (latencyData.avg_latency <= 100) {
    return (
      <span className="flex items-center justify-center">
        <FaCheck className="mr-xs" />
        Connection
      </span>
    );
  }

  if (latencyData.avg_latency > 100 && latencyData.avg_latency <= 200) {
    return (
      <span className="flex items-center justify-center">
        <FaMinusCircle className="mr-xs" />
        Acceptable
      </span>
    );
  }

  return (
    <span className="flex items-center justify-center">
      <FaExclamationTriangle className="mr-xs" />
      Elevated Latency
    </span>
  );
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  dataSource,
  dbVersion,
  dbLatencyData,
}) => {
  const [isOpen, setIsOpen] = useState(false);

  const toggleHandler = () => setIsOpen(prev => !prev);

  return (
    <div>
      <div onClick={toggleHandler} role="button" tabIndex={0}>
        <div className="flex items-center">
          <div className="text-gray-600 font-semibold mr-xs break-all">
            {dataSource.name}{' '}
            <span className="font-normal mr-px">
              ({driverToLabel[dataSource.driver]})
            </span>
            {dbLatencyData &&
              (isNumber(dbLatencyData.avg_latency) ||
                isFloat(dbLatencyData.avg_latency)) &&
              Math.ceil(dbLatencyData.avg_latency) > 0 && (
                <ToolTip
                  message={`Latency: ${Math.ceil(
                    dbLatencyData.avg_latency
                  )} ms`}
                  placement="top"
                >
                  <Badge color={getBadgeColorForLatencyData(dbLatencyData)}>
                    {getBadgeTextForLatencyData(dbLatencyData)}
                  </Badge>
                </ToolTip>
              )}
          </div>
          <div className="flex ml-auto w-[30%] sm:w-1/4">
            {!!dataSource?.read_replicas?.length && (
              <span className="mr-xs inline-flex items-center px-2.5 py-0.5 rounded-full text-sm font-medium bg-blue-100 text-gray-800">
                {dataSource.read_replicas.length} Replicas
              </span>
            )}
            <span className="ml-auto text-sm">
              {isOpen ? <FaChevronUp /> : <FaChevronDown />}{' '}
              {isOpen ? <span>Less Info</span> : <span>More Info</span>}
            </span>
          </div>
        </div>
      </div>

      {isOpen ? (
        <div className="mt-xs space-y-xs">
          {dbVersion ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold">Database</div>
              <div className="ml-auto w-full sm:w-6/12">{dbVersion}</div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.max_connections ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold mr-xs">
                Max Connections
              </div>
              <div className="ml-auto w-full sm:w-6/12">
                {dataSource.connection_pool_settings?.max_connections}
              </div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.idle_timeout ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold mr-xs">
                Idle Timeout
              </div>
              <div className="ml-auto w-full sm:w-6/12">
                {dataSource.connection_pool_settings?.idle_timeout}
              </div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.retries ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold mr-xs">Retries</div>
              <div className="ml-auto w-full sm:w-6/12">
                {dataSource.connection_pool_settings?.retries}
              </div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.pool_timeout ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold mr-xs">
                Pool Timeout
              </div>
              <div className="ml-auto w-full sm:w-6/12">
                {dataSource.connection_pool_settings?.pool_timeout}
              </div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.connection_lifetime ? (
            <div className="flex content-start">
              <div className="text-gray-600 font-semibold mr-xs">
                Connection Lifetime
              </div>
              <div className="ml-auto w-full sm:w-6/12">
                {dataSource.connection_pool_settings?.connection_lifetime}
              </div>
            </div>
          ) : null}
        </div>
      ) : (
        ''
      )}
    </div>
  );
};

export default CollapsibleToggle;
