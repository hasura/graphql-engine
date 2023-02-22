import React from 'react';
import { useRemoteDatabaseRelationships } from '../../../../../features/MetadataAPI';
import { NormalizedTable } from '../../../../../dataSources/types';
import { Dispatch } from '../../../../../types';
import { currentDriver } from '../../../../../dataSources';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';

import styles from '../../TableModify/ModifyTable.module.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import AddManualRelationship from './AddManualRelationship';

type Props = {
  tableSchema: NormalizedTable;
  reduxDispatch: Dispatch;
  currentSource: string;
};

export const RemoteDbRelationships: React.FC<Props> = ({
  tableSchema,
  reduxDispatch,
  currentSource,
}) => {
  const target = React.useMemo(() => {
    if (currentDriver === 'bigquery') {
      return {
        database: currentSource,
        table: tableSchema.table_name,
        dataset: tableSchema.table_schema,
      };
    }

    return {
      database: currentSource,
      table: tableSchema.table_name,
      schema: tableSchema.table_schema,
    };
  }, [currentSource, tableSchema]);

  const { isLoading, isError, isSuccess, data } =
    useRemoteDatabaseRelationships(target); // get data from hook

  if (isLoading) {
    return <p>Loading</p>;
  }

  if (isError) {
    return <p>Error</p>;
  }

  return (
    <>
      {isSuccess ? (
        <>
          <h4 className={styles.subheading_text}>
            Remote Database Relationships
            <ToolTip message="Relationships to remote database tables" />
            <LearnMoreLink
              href="https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/remote-relationships/remote-source-relationships.html"
              className="font-normal"
            />
          </h4>
          <div className={styles.activeEdit}>
            {data?.map(r => (
              <AddManualRelationship
                key={r?.name}
                tableSchema={tableSchema}
                reduxDispatch={reduxDispatch}
                currentSource={currentSource}
                relationship={r}
              />
            ))}
            <AddManualRelationship
              tableSchema={tableSchema}
              currentSource={currentSource}
              reduxDispatch={reduxDispatch}
              relationship={null}
            />
          </div>
        </>
      ) : null}
    </>
  );
};
