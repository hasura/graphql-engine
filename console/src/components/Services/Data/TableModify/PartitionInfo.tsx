import React, { useEffect, useState } from 'react';
import { FaColumns, FaTable } from 'react-icons/fa';
import { Table, Partition } from '../../../../dataSources/types';
import { Dispatch } from '../../../../types';
import { fetchPartitionDetails } from '../DataActions';

interface Props {
  table: Table;
  dispatch: Dispatch;
}

const HighlightedText = ({ value }: { value: string }) => {
  let insideQuotes = false;
  return (
    <span>
      {value.split('').map(k => {
        let res = <span>{k}</span>;
        if (k === `'` || k === `"`) {
          res = <span>{k}</span>;
          insideQuotes = !insideQuotes;
        }
        if (insideQuotes) {
          res = <span>{k}</span>;
        }
        return res;
      })}
    </span>
  );
};

const PartitionInfo: React.FC<Props> = ({ table, dispatch }) => {
  const [partitions, setPartitions] = useState<Record<string, Partition[]>>({});

  useEffect(() => {
    dispatch(fetchPartitionDetails(table)).then((data: Partition[]) => {
      const partitionsMap = {} as Record<string, Partition[]>;
      const unqiuePKs = data
        .map(p => p.partition_key)
        .filter((elem, index, self) => {
          return index === self.indexOf(elem);
        });

      unqiuePKs.forEach(t => {
        const related = data.filter(x => x.partition_key === t);
        partitionsMap[t] = related;
      });

      setPartitions(partitionsMap);
    });
  }, []);

  return (
    <div className="mb-md">
      {partitions && Object.keys(partitions).length > 0 && (
        <>
          <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
            Partitions
          </h4>
          {Object.keys(partitions).map(key => (
            <div>
              <span className="font-semibold mr-xs">
                <FaColumns aria-hidden="true" />
                created_at -{' '}
              </span>
              <span className="mr-xs">{key}</span>
              {partitions[key].map(p => {
                return (
                  <div className="mt-sm">
                    <span className="font-semibold mr-xs">
                      <FaTable className="mr-xs" aria-hidden="true" />{' '}
                      {p.partition_name} -{' '}
                    </span>
                    <HighlightedText value={p.partition_def} />
                  </div>
                );
              })}
            </div>
          ))}
        </>
      )}
    </div>
  );
};

export default PartitionInfo;
