import { Tooltip } from '@/new-components/Tooltip';
import React from 'react';
import { FaAngleRight, FaDatabase } from 'react-icons/fa';
import { RiInformationFill } from 'react-icons/ri';
import { TrackTables } from '../TrackTables/TrackTables';

interface ManageDatabaseProps {
  dataSourceName: string;
}

export const ManageDatabase = (props: ManageDatabaseProps) => {
  return (
    <div className="w-full overflow-y-auto bg-gray-50">
      <div className="px-md pt-md mb-xs">
        <div className="flex items-center space-x-xs mb-1">
          <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
            <FaDatabase className="mr-1.5" />
            <span className="text-sm">default</span>
          </div>
          <FaAngleRight className="text-muted" />
          <div className="cursor-pointer flex items-center">
            <span className="text-sm font-semibold text-yellow-500">
              Manage
            </span>
          </div>
        </div>

        <div className="flex items-center">
          <div className="group relative">
            <h1 className="inline-flex items-center text-xl font-semibold mb-1">
              {props.dataSourceName}
            </h1>
          </div>
        </div>
      </div>
      <div>
        <div className="px-md group relative">
          <div className="flex mb-1 items-center">
            <h4 className="inline-flex items-center font-semibold">
              Track Tables
            </h4>
            <Tooltip
              tooltipContentChildren="Expose the tables available in your database via the GraphQL API"
              side="right"
            >
              <RiInformationFill />
            </Tooltip>
          </div>

          <p className="text-muted">
            Manage your database Tracking objects adds them to your GraphQL API.
            All objects will be admin-only until permissions have been set.
          </p>
        </div>
        <TrackTables dataSourceName={props.dataSourceName} />
      </div>
    </div>
  );
};
