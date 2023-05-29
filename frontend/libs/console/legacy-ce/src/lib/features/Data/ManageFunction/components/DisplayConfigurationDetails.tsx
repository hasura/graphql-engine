import Skeleton from 'react-loading-skeleton';
import {
  MetadataSelectors,
  areTablesEqual,
  useMetadata,
} from '../../../hasura-metadata-api';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Badge } from '../../../../new-components/Badge';
import { TableDisplayName } from '../../ManageTable/components/TableDisplayName';
import { QualifiedFunction } from '../../../hasura-metadata-types';

export type DisplayConfigurationDetailsProps = {
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
};
export const DisplayConfigurationDetails = (
  props: DisplayConfigurationDetailsProps
) => {
  const {
    data: metadataFunction,
    isLoading,
    error,
  } = useMetadata(m =>
    MetadataSelectors.findSource(props.dataSourceName)(m)?.functions?.find(f =>
      areTablesEqual(f.function, props.qualifiedFunction)
    )
  );

  if (isLoading)
    return (
      <div className="mx-sm">
        <Skeleton count={8} height={25} className="mb-2" />
      </div>
    );

  if (!metadataFunction)
    return (
      <IndicatorCard
        status="negative"
        headline="Could not find function in metadata"
      >
        {JSON.stringify(error)}
      </IndicatorCard>
    );

  return (
    <div>
      <div className="flex gap-4 mb-sm">
        <b>Custom Name: </b>
        {metadataFunction.configuration?.custom_name ?? <Badge>Not Set</Badge>}
      </div>
      <div className="flex gap-4">
        <b>Return Type: </b>
        <TableDisplayName
          table={metadataFunction.configuration?.response?.table}
        />
      </div>
    </div>
  );
};
