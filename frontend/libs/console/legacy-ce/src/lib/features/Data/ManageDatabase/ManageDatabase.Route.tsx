import React from 'react';
import { useTableDefinition } from '../hooks';
import { ManageDatabase } from './ManageDatabase';

export const ManageDatabaseRoute = () => {
  const urlData = useTableDefinition(window.location);

  if (urlData.querystringParseResult === 'error')
    return <>Something went wrong while parsing the URL parameters</>;

  const { database } = urlData.data;

  /**
   * If the url is only managing a database and not the table, then show the DB-management screen
   */
  return <ManageDatabase dataSourceName={database} />;
};
