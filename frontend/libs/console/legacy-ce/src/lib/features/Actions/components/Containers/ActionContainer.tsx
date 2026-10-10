import React from 'react';
import { useNavigate } from 'react-router';
import actionTabs from './actionTabs';
import { BreadcrumbItem, Breadcrumbs, Tabs } from '@hasura/shared/ui';
import { useCurrentActionContext } from '../../context';
import { Heading } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';

type Props = {
  tabName: string;
  children: React.ReactNode;
};

const ActionContainer = ({ tabName, children }: Props) => {
  const navigate = useNavigate();
  const { currentAction } = useCurrentActionContext();
  const actionName = currentAction.name;

  const breadCrumbs: BreadcrumbItem[] = [
    {
      title: 'Actions',
      url: `${dataRoutes.manageActions}/actions`,
    },
    {
      title: actionName,
      url: dataRoutes.manageAction(actionName, 'modify'),
    },
    {
      title: tabName,
    },
  ];

  return (
    <div className="mt-6">
      <Breadcrumbs items={breadCrumbs} />
      <div className="my-4">
        <Heading size="4">{actionName}</Heading>
      </div>
      <Tabs
        value={tabName}
        onValueChange={(newTab) =>
          navigate(dataRoutes.manageAction(actionName, newTab))
        }
        items={Object.entries(actionTabs).map(([value, { display_text }]) => ({
          value,
          label: display_text,
          content:
            value === tabName ? <div className="mt-4">{children}</div> : null,
        }))}
      />
    </div>
  );
};

export default ActionContainer;
