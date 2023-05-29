import { QualifiedFunction } from '../../hasura-metadata-types';
import { Tabs } from '../../../new-components/Tabs';
import { getRoute } from '..';
import React from 'react';
import { useDispatch } from 'react-redux';
import _push from '../../../components/Services/Data/push';
import { useURLParameters } from './hooks/useUrlParameters';
import { Breadcrumbs } from './components/Breadcrumbs';
import { Heading } from './components/Heading';
import { Modify } from './components/Modify';

type AllowedTabs = 'modify';
export interface ManageFunctionProps {
  params: {
    operation: AllowedTabs;
  };
}

type Tab = {
  value: string;
  label: string;
  content: JSX.Element;
};

const availableTabs = (
  dataSourceName: string,
  qualifiedFunction: QualifiedFunction
): Tab[] => [
  {
    value: 'modify',
    label: 'Modify',
    content: (
      <Modify
        qualifiedFunction={qualifiedFunction}
        dataSourceName={dataSourceName}
      />
    ),
  },
];

export const ManageFunction: React.VFC<ManageFunctionProps> = (
  props: ManageFunctionProps
) => {
  const {
    params: { operation },
  } = props;

  const urlData = useURLParameters(window.location);
  const dispatch = useDispatch();

  if (urlData.querystringParseResult === 'error')
    throw Error('Unable to render');

  const { database: dataSourceName, function: qualifiedFunction } =
    urlData.data;

  return (
    <div className="w-full bg-gray-50">
      <div className="px-md pt-md mb-xs">
        <Breadcrumbs
          dataSourceName={dataSourceName}
          qualifiedFunction={qualifiedFunction}
        />
        <Heading
          dataSourceName={dataSourceName}
          qualifiedFunction={qualifiedFunction}
        />
        <Tabs
          value={operation}
          onValueChange={_operation => {
            dispatch(
              _push(getRoute().function(dataSourceName, qualifiedFunction))
            );
          }}
          items={availableTabs(dataSourceName, qualifiedFunction)}
        />
      </div>
    </div>
  );
};
