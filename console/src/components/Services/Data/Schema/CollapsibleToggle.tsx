import React, { useState } from 'react';
import { FaChevronDown, FaChevronUp } from 'react-icons/fa';

import { driverToLabel } from '../../../../dataSources';
import { DataSource } from '../../../../metadata/types';

type CollapsibleToggleProps = {
  dataSource: DataSource;
  dbVersion: string;
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  dataSource,
  dbVersion,
}) => {
  const [isOpen, setIsOpen] = useState(false);

  const toggleHandler = () => setIsOpen(prev => !prev);

  return (
    <div>
      <div onClick={toggleHandler} role="button" tabIndex={0}>
        <div className="flex items-center">
          <div className="text-gray-600 font-semibold mr-xs break-all">
            {dataSource.name}
          </div>
          <div className="flex ml-auto w-full sm:w-6/12">
            <span className="mr-xs">({driverToLabel[dataSource.driver]})</span>
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
        <>
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
        </>
      ) : (
        ''
      )}
    </div>
  );
};

export default CollapsibleToggle;
