import { useNavigate, useParams } from 'react-router';
import { IndicatorCard, Tabs } from '@hasura/shared/ui';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { LogicalModel } from '@hasura/shared/types';
import { LogicalModelPermissionsPage } from '../../../Permissions/LogicalModelPermissions/LogicalModelPermissionsPage';
import { MetadataWrapper } from '../../components';
import { LogicalModelWidget } from '../LogicalModelWidget/LogicalModelWidget';
import { RouteWrapper } from '../components/RouteWrapper';
import { injectRouteDetails } from '../components/route-wrapper-utils';
import { Routes } from '../constants';
import { LogicalModelTabs } from '../types';
import { logicalModelFieldToFormField } from './utils/logicalModelFieldToFormField';

export const LogicalModelRoute = () => {
  const params = useParams<{
    source: string;
    name: string;
    tabName?: LogicalModelTabs;
  }>();
  const { source, name, tabName } = params;
  if (!source || !name) {
    return (
      <IndicatorCard status="negative">
        Unable to parse data from url.
      </IndicatorCard>
    );
  }

  return (
    <MetadataWrapper
      selector={MetadataSelectors.findLogicalModel(source, name)}
      render={({ data: logicalModel }) => (
        <LogicalModelLandingPage
          source={source}
          name={name}
          tabName={tabName}
          logicalModel={logicalModel}
        />
      )}
    />
  );
};

const LogicalModelLandingPage = ({
  name: logicalModelName,
  source,
  tabName,
  logicalModel,
}: {
  name: string;
  source: string;
  tabName?: string;
  logicalModel: LogicalModel | undefined;
}) => {
  const push = useNavigate();

  if (!logicalModel) {
    return (
      <IndicatorCard status="negative">
        Logical Model {logicalModelName} not found in {source}
      </IndicatorCard>
    );
  }

  return (
    <RouteWrapper
      route={Routes.EditLogicalModel}
      itemSourceName={source}
      itemName={logicalModelName}
      itemTabName={tabName}
      subtitle={
        tabName === 'details'
          ? 'Make changes to your Logical Model'
          : 'Add permissions to your Logical Models to control access to your data'
      }
    >
      <Tabs
        data-testid="logical-model-tabs"
        defaultValue={tabName ?? 'details'}
        onValueChange={(tab) =>
          push(
            injectRouteDetails(Routes.EditLogicalModel, {
              itemName: logicalModel.name,
              itemSourceName: source,
              itemTabName: tab,
            }),
          )
        }
        items={[
          {
            value: 'details',
            label: `Logical Model`,
            content: (
              <div className="py-4 max-w">
                <LogicalModelWidget
                  key={`${source}-${logicalModelName}`}
                  disabled={{
                    dataSourceName: true,
                  }}
                  defaultValues={{
                    name: logicalModel.name,
                    dataSourceName: source,
                    fields: logicalModel.fields.map(
                      logicalModelFieldToFormField,
                    ),
                  }}
                  onSubmit={() => {
                    push(Routes.LogicalModels);
                  }}
                />
              </div>
            ),
          },
          {
            value: 'permissions',
            label: `Permissions`,
            content: (
              <LogicalModelPermissionsPage
                key={`${source}-${logicalModelName}`}
                source={source}
                name={logicalModelName}
              />
            ),
          },
        ]}
      />
    </RouteWrapper>
  );
};
