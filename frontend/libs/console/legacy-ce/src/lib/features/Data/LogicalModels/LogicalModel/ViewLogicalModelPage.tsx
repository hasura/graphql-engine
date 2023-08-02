import { InjectedRouter, withRouter } from 'react-router';
import { RouteWrapper } from '../components/RouteWrapper';
import { LogicalModelTabs } from '../components/LogicalModelTabs';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { MetadataUtils, useMetadata } from '../../../hasura-metadata-api';
import Skeleton from 'react-loading-skeleton';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { logicalModelFieldToFormField } from './utils/logicalModelFieldToFormField';

export const ViewLogicalModelRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
  params: {
    source: string;
    name: string;
  };
}>(({ params }) => {
  if (!params.source || !params.name) {
    return (
      <IndicatorCard status="negative">
        Unable to parse data from url.
      </IndicatorCard>
    );
  }
  return (
    <RouteWrapper
      route={'/data/native-queries/logical-models/{{source}}/{{name}}'}
      itemSourceName={params.source}
      itemName={params.name}
    >
      <LogicalModelTabs
        source={params.source}
        name={params.name}
        defaultValue="logical-model"
      />
    </RouteWrapper>
  );
});

export const ViewLogicalModelPage = ({
  source,
  name,
}: {
  source: string;
  name: string;
}) => {
  const { data: logicalModels, isLoading } = useMetadata(
    m => MetadataUtils.findMetadataSource(source, m)?.logical_models
  );

  if (isLoading) {
    return (
      <div>
        <Skeleton count={10} />
      </div>
    );
  }

  const logicalModel = logicalModels?.find(l => l.name === name);

  if (!logicalModel) {
    return (
      <IndicatorCard status="negative">
        Logical Model {name} not found in {source}
      </IndicatorCard>
    );
  }

  return (
    <div className="py-md max-w">
      <LogicalModelWidget
        disabled={{
          name: true,
          dataSourceName: true,
          fields: true,
          callToAction: true,
        }}
        defaultValues={{
          name: logicalModel.name,
          dataSourceName: source,
          fields: logicalModel.fields.map(logicalModelFieldToFormField),
        }}
      />
    </div>
  );
};
