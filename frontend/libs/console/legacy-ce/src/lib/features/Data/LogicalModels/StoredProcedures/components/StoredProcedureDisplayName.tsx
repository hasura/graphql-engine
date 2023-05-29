import { QualifiedStoredProcedure } from '../../../../hasura-metadata-types';
import { getQualifiedTable } from '../../../ManageTable/utils';
import { Link } from '../../../TrackResources/components/parts/Link';
import { TbFileSettings } from 'react-icons/tb';

export const StoredProcedureDisplayName = ({
  dataSourceName,
  qualifiedStoredProcedure,
  onClick,
}: {
  onClick?: () => void;
  dataSourceName?: string;
  qualifiedStoredProcedure: QualifiedStoredProcedure;
}) => {
  const qualifiedStoredProcedureName = getQualifiedTable(
    qualifiedStoredProcedure
  );
  const content = () => (
    <span className="flex items-center">
      <TbFileSettings className="text-2xl text-muted mr-xs" />
      {dataSourceName ? (
        <>
          {dataSourceName} / {qualifiedStoredProcedureName.join(' / ')}
        </>
      ) : (
        <>{qualifiedStoredProcedureName.join(' / ')}</>
      )}
    </span>
  );

  return onClick ? (
    <Link onClick={onClick}>{content()}</Link>
  ) : (
    <div>{content()}</div>
  );
};
