import clsx from 'clsx';
import { useState } from 'react';
import { FaAngleDown, FaAngleUp } from 'react-icons/fa';

export type SourceLevelSummary = {
  dataSourceName: string;
  totalCount: number;
};

export type Props = {
  tablesAndViews?: SourceLevelSummary[];
  collections?: SourceLevelSummary[];
  logicalModels: SourceLevelSummary[];
  isOssMode?: boolean;
};

const calculateTotal = (items: SourceLevelSummary[]) => {
  return items.reduce((acc, item) => acc + item.totalCount, 0);
};

const SummaryView = ({
  label,
  items,
}: {
  label: string;
  items: SourceLevelSummary[];
}) => {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div>
      <div className="flex justify-between items-center p-4">
        <span className="text-gray-700 font-semibold">{label}</span>
        <div className="flex items-center gap-1.5">
          <span className="bg-gray-200 px-2 rounded">
            {calculateTotal(items)}
          </span>
          <span
            onClick={() => setIsOpen(!isOpen)}
            className={clsx(
              !items.length ? 'cursor-not-allowed text-gray-400' : ''
            )}
          >
            {isOpen ? <FaAngleUp /> : <FaAngleDown />}
          </span>
        </div>
      </div>

      {isOpen &&
        items.map(source => {
          return (
            <div className="flex justify-between items-center p-4 pl-16 pr-10">
              <span className="text-gray-700">{source.dataSourceName}</span>
              <span>{source.totalCount}</span>
            </div>
          );
        })}
    </div>
  );
};

export const ModelSummary = ({
  tablesAndViews = [],
  collections = [],
  logicalModels = [],
  isOssMode = false,
}: Props) => {
  return (
    <div className="my-2">
      <div className="py-4 px-8">
        <div className="font-semibold text-3xl text-gray-700 ">
          Model Count Summary
        </div>

        <p className="subtitle">
          The summary of all your tables, views, collections and logical models
          tracked in your metadata
        </p>
      </div>

      <hr className="my-1.5" />

      <div className="mx-6 my-2 bg-white rounded border">
        <div className="flex justify-between items-center p-4">
          <span className="text-2xl text-gray-700">Total Number of Models</span>
          <span className="bg-amber-100 px-2 rounded">
            {calculateTotal(tablesAndViews) +
              calculateTotal(collections) +
              calculateTotal(logicalModels)}
          </span>
        </div>

        <hr className="my-1.5" />

        <SummaryView label={'Tables and Views'} items={tablesAndViews} />

        {!isOssMode && (
          <SummaryView label={'Collections'} items={collections} />
        )}

        <SummaryView label={'Logical Models'} items={logicalModels} />
      </div>
    </div>
  );
};
