import { Breadcrumbs, Tabs } from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Heading } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';

const CustomTypesContainer = ({ children, tabName }) => {
  const breadCrumbs = [
    {
      title: 'Actions',
      url: dataRoutes.manageActions,
    },
    {
      title: 'Types',
      url: dataRoutes.manageAction('types'),
    },
    {
      title: tabName,
    },
  ];

  return (
    <Analytics name="CustomTypesContainer" {...REDACT_EVERYTHING}>
      <div className="mt-6">
        <Breadcrumbs items={breadCrumbs} />
        <div className="my-4">
          <Heading size="5">Custom Types</Heading>
        </div>
        <Tabs
          value={tabName}
          items={[{ value: 'manage', label: 'Manage', content: null }]}
        />
        <div className="pt-6">{children}</div>
      </div>
    </Analytics>
  );
};

export default CustomTypesContainer;
