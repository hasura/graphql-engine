import React from 'react';
import { EventTrigger } from '../../types';

type ETInfoProps = {
  currentTrigger: EventTrigger;
};

const tdHeadStyle = 'px-3 py-3 whitespace-nowrap font-medium';
const tdValStyle = 'px-3 py-3 whitespace-nowrap text-gray-600';

const Info = ({ currentTrigger }: ETInfoProps) => (
  <div className="flex flex-col mb-lg w-6/12">
    <div className="overflow-x-auto border border-gray-300 rounded mb-sm">
      <table className="min-w-full divide-y divide-gray-200">
        <tbody className="bg-white divide-y divide-gray-200">
          <tr className="">
            <td className={tdHeadStyle}>Trigger Name</td>
            <td className={tdValStyle}>{currentTrigger.name}</td>
          </tr>
          <tr className="">
            <td className={tdHeadStyle}>Table</td>
            <td className={tdValStyle}>{currentTrigger.table_name}</td>
          </tr>
          <tr className="">
            <td className={tdHeadStyle}>Schema</td>
            <td className={tdValStyle}>{currentTrigger.schema_name}</td>
          </tr>
          <tr className="">
            <td className={tdHeadStyle}>Data Source</td>
            <td className={tdValStyle}>{currentTrigger.source}</td>
          </tr>
        </tbody>
      </table>
    </div>
    <p className="text-sm text-gray-600">
      *Remove this trigger and create a new one to replace these options.
    </p>
  </div>
);

export default Info;
