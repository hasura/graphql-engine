import { MetadataFunction, Source } from '@hasura/shared/types';
import { Tabs, Breadcrumbs, IndicatorCard } from '@hasura/shared/ui';
import React, { type JSX } from 'react';
import { useNavigate, useParams } from 'react-router';
import { useFunctionURLParameters } from './hooks/useUrlParameters';
import { Heading } from './components/Heading';
import { Modify } from './components/Modify';
import { FaDatabase } from 'react-icons/fa';
import { TbMathFunction } from 'react-icons/tb';
import { areTablesEqual, functionDisplayName } from '@hasura/metadata/helpers';
import { dataRoutes } from '@hasura/shared/utils';
import { useDataSourceContext } from '../context/DataSourceContext';
import {
  GetFunctionDefinitionResult,
  useFunctionDefinition,
} from '@hasura/metadata/data-source';
import FunctionPermissions from '../../Permissions/FunctionPermissions/Permission';

type AllowedTabs = 'modify';

type Tab = {
  value: string;
  label: string;
  content: JSX.Element;
};

const availableTabs = (
  source: Source,
  currentFunction: MetadataFunction,
  functionDefinition: GetFunctionDefinitionResult | null | undefined,
  refetchFunctionDefinition?: () => void,
): Tab[] =>
  [
    {
      value: 'modify',
      label: 'Modify',
      content: (
        <Modify
          currentFunction={currentFunction}
          source={source}
          functionDefinition={functionDefinition}
          refetchFunctionDefinition={refetchFunctionDefinition}
        />
      ),
    },
  ].concat(
    functionDefinition?.returnTable || currentFunction.configuration?.response
      ? [
          {
            value: 'permissions',
            label: 'Permissions',
            content: (
              <FunctionPermissions
                currentFunction={currentFunction}
                source={source}
                functionDefinition={functionDefinition}
              />
            ),
          },
        ]
      : [],
  );

export const ManageFunction: React.FC = () => {
  const { currentSource } = useDataSourceContext();
  const urlData = useFunctionURLParameters();

  const currentFunction: MetadataFunction | undefined =
    urlData.querystringParseResult === 'success' && urlData.qualifiedFunction
      ? currentSource.functions?.find((fn) =>
          areTablesEqual(fn.function, urlData.qualifiedFunction!),
        )
      : undefined;

  if (!currentFunction) {
    return (
      <div className="p-6">
        <IndicatorCard status="negative" showIcon>
          Function not found
        </IndicatorCard>
      </div>
    );
  }

  return (
    <ManageFunctionTabs
      currentFunction={currentFunction}
      source={currentSource}
    />
  );
};

export const ManageFunctionTabs = ({
  source,
  currentFunction,
}: {
  source: Source;
  currentFunction: MetadataFunction;
}) => {
  const navigate = useNavigate();
  const { operation } = useParams<{
    operation: AllowedTabs;
  }>();

  const { data: functionDefinition, refetch: refetchFunctionDefinition } =
    useFunctionDefinition({
      func: currentFunction.function,
      source,
    });

  return (
    <div className="w-full p-6">
      <Breadcrumbs
        items={[
          { title: source.name, icon: <FaDatabase /> },
          {
            title: functionDisplayName({
              dataSourceName: source.name,
              qualifiedFunction: currentFunction.function,
            }),
            icon: <TbMathFunction className="text-muted mr-1" />,
          },
          'Manage',
        ]}
      />
      <Heading source={source} qualifiedFunction={currentFunction.function} />
      <Tabs
        value={operation}
        onValueChange={(_operation) => {
          navigate(
            dataRoutes.manageFunction(
              source.name,
              currentFunction.function!,
              _operation,
            ),
          );
        }}
        items={availableTabs(
          source,
          currentFunction,
          functionDefinition,
          refetchFunctionDefinition,
        )}
      />
    </div>
  );
};
