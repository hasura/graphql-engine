import { useMetadata } from '../../hasura-metadata-api';
import React from 'react';
import DbConnectSVG from '../graphics/database-connect.svg';
export const ConnectDatabaseWrapper: React.FC = ({ children }) => {
  const { data: metadataSources } = useMetadata(m => m.metadata.sources);

  return (
    <div className="flex flex-col items-center">
      <div className="py-lg border-b border-slate-300 w-full flex justify-center">
        <div className="max-w-3xl w-full">
          <div className="text-xl font-bold">
            {metadataSources?.length
              ? 'Connect Database'
              : 'Connect Your First Database'}
          </div>
          {metadataSources?.length ? (
            <div className="text-muted">
              Connect a database to access your database objects in your GraphQL
              API.
            </div>
          ) : (
            <div className="text-muted">
              Connect your first database to access your database objects in
              your GraphQL API.
            </div>
          )}
        </div>
      </div>
      <div className="max-w-3xl py-lg w-full">
        <div className="flex flex-col">
          <img
            src={DbConnectSVG}
            className={`mb-md w-full`}
            alt="Database Connection Diagram"
          />
          {children}
        </div>
      </div>
    </div>
  );
};
