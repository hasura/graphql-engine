import { QualifiedStoredProcedure } from '@hasura/shared/types';
import { getQualifiedTable } from '../../../ManageTable/utils';
import { TbFileSettings } from 'react-icons/tb';
import { To } from 'react-router';
import { RelativeLink } from '@hasura/shared/ui';

export const StoredProcedureDisplayName = ({
  dataSourceName,
  qualifiedStoredProcedure,
  to,
}: {
  to?: To;
  dataSourceName?: string;
  qualifiedStoredProcedure: QualifiedStoredProcedure;
}) => {
  const qualifiedStoredProcedureName = getQualifiedTable(
    qualifiedStoredProcedure,
  );
  const content = () => (
    <span className="flex items-center">
      <TbFileSettings className="text-2xl text-muted mr-1" />
      {dataSourceName ? (
        <>
          {dataSourceName} / {qualifiedStoredProcedureName.join(' / ')}
        </>
      ) : (
        <>{qualifiedStoredProcedureName.join(' / ')}</>
      )}
    </span>
  );

  return to ? (
    <RelativeLink to={to}>{content()}</RelativeLink>
  ) : (
    <div>{content()}</div>
  );
};
