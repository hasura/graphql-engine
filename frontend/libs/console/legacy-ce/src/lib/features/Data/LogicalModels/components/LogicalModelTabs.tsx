import { Tabs } from '../../../../new-components/Tabs';
import { LogicalModelPermissionsPage } from '../../../Permissions/LogicalModelPermissions/LogicalModelPermissionsPage';
import { ViewLogicalModelPage } from '../LogicalModel/ViewLogicalModelPage';

export const LogicalModelTabs = ({
  defaultValue,
  source,
  name,
}: {
  defaultValue: 'logical-model' | 'logical-model-permissions';
  source: string;
  name: string;
}) => {
  return (
    <Tabs
      data-testid="logical-model-tabs"
      defaultValue={defaultValue}
      items={[
        {
          value: 'logical-model',
          label: `Logical Model`,
          content: <ViewLogicalModelPage source={source} name={name} />,
        },
        {
          value: 'logical-model-permissions',
          label: `Permissions`,
          content: <LogicalModelPermissionsPage source={source} name={name} />,
        },
      ]}
    />
  );
};
