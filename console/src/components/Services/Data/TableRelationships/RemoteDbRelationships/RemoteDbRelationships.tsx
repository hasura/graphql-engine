import React from 'react';
import { useRemoteDatabaseRelationships } from '@/features/MetadataAPI';
import { NormalizedTable } from '@/dataSources/types';
import { Dispatch } from '@/types';
import { currentDriver } from '@/dataSources';
import { QualifiedTable } from '@/metadata/types';
import styles from '../../TableModify/ModifyTable.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';
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
  let qualifiedTable = {};
  if (currentDriver !== 'bigquery') {
    qualifiedTable = {
      name: tableSchema.table_name,
      schema: tableSchema.table_schema,
    };
  } else {
    qualifiedTable = {
      name: tableSchema.table_name,
      dataset: tableSchema.table_schema,
    };
  }

  const {
    isLoading,
    isError,
    isSuccess,
    data,
  } = useRemoteDatabaseRelationships(
    currentSource,
    qualifiedTable as QualifiedTable
  ); // get data from hook

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
            &nbsp;
            <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/databases/postgres/schema/remote-relationships/remote-source-relationships.html" />
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
