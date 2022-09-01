import React from 'react';
import { useTableDefinition } from './hooks';
import { ManageDatabase } from './ManageDatabase/ManageDatabase';

export const ManageContainer = () => {
  const urlData = useTableDefinition();

  if (urlData.querystringParseResult === 'error')
    return <>Something went wrong while parsing the URL parameters</>;

  const { database, table } = urlData.data;

  /**
   * If the url is only managing a database and not the table, then show the DB-management screen
   */
  if (database && !table) return <ManageDatabase dataSourceName={database} />;

  return <>Feature not implemented</>;
};
