import React, { useEffect, useState } from 'react';
import { Table, Partition } from '../../../../dataSources/types';
import { Dispatch } from '../../../../types';
import { fetchPartitionDetails } from '../DataActions';
import styles from './ModifyTable.scss';

interface Props {
  table: Table;
  dispatch: Dispatch;
}

const HighlightedText = ({ value }: { value: string }) => {
  let insideQuotes = false;
  return (
    <span className={styles.defText}>
      {value.split('').map(k => {
        let res = <span>{k}</span>;
        if (k === `'` || k === `"`) {
          res = <span className={styles.quoteText}>{k}</span>;
          insideQuotes = !insideQuotes;
        }
        if (insideQuotes) {
          res = <span className={styles.quoteText}>{k}</span>;
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
    <div>
      {partitions && Object.keys(partitions).length > 0 && (
        <>
          <h4 className={styles.subheading_text}>Partitions</h4>
          {Object.keys(partitions).map(key => (
            <div>
              <b>
                <i
                  className={`fa fa-columns ${styles.partitionLabel}`}
                  aria-hidden="true"
                />
                created_at -{' '}
              </b>
              <i>{key}</i>
              {partitions[key].map(p => {
                return (
                  <div
                    className={`${styles.paddingTopSm} ${styles.partitionDef}`}
                  >
                    <b>
                      <i
                        className={`fa fa-table ${styles.partitionLabel}`}
                        aria-hidden="true"
                      />{' '}
                      {p.partition_name} -{' '}
                    </b>
                    <HighlightedText value={p.partition_def} />
                  </div>
                );
              })}
            </div>
          ))}
          <hr className="my-md" />
        </>
      )}
    </div>
  );
};

export default PartitionInfo;
