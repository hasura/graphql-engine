import {
  Badge,
  DataList,
  IconTooltip,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { MetadataFunction, QualifiedDataSource } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import { GetFunctionDefinitionResult } from '@hasura/metadata/data-source';
import { TableDisplayName } from '../../ManageTable/components/TableDisplayName';
import { dataRoutes } from '@hasura/shared/utils';

export type DisplayConfigurationDetailsProps = {
  source: QualifiedDataSource;
  currentFunction: MetadataFunction;
  functionDefinition: GetFunctionDefinitionResult | null | undefined;
};

export const DisplayConfigurationDetails = ({
  source,
  currentFunction,
  functionDefinition,
}: DisplayConfigurationDetailsProps) => {
  const notSetBadge = <Badge>Not Set</Badge>;
  const returnType =
    currentFunction.configuration?.response?.table ??
    functionDefinition?.returnTable;

  return (
    <>
      <Flex gap="2" align="center">
        <Text size="2" weight="bold">
          Custom Field Names
        </Text>
        <IconTooltip message="Allows you to customize any given function with a custom name and custom root fields of an already tracked function. This will replace the already present customization." />
        <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#custom-function-root-fields" />
      </Flex>
      <DataList
        size="2"
        items={[
          {
            label: 'Return Type:',
            value: returnType ? (
              <TableDisplayName
                table={returnType}
                to={dataRoutes.manageTable(source.name, returnType)}
              />
            ) : (
              <Badge>Not Set</Badge>
            ),
          },
          {
            label: 'Custom Name:',
            value: currentFunction.configuration?.custom_name ?? (
              <Badge>Not Set</Badge>
            ),
          },
          {
            label: 'Root Field:',
            value:
              currentFunction.configuration?.custom_root_fields?.function ??
              notSetBadge,
          },
          {
            label: 'Aggregate Root Field:',
            value:
              currentFunction.configuration?.custom_root_fields
                ?.function_aggregate ?? notSetBadge,
          },
          {
            label: 'Exposed As:',
            value: currentFunction.configuration?.exposed_as ?? 'query',
          },
        ]}
      />

      <Flex gap="2" align="center">
        <Text size="2" weight="bold">
          Session Argument
        </Text>
        <IconTooltip message="the function argument into which hasura session variables will be passed" />
        <LearnMoreLink href="https://hasura.io/docs/2.0/schema/postgres/custom-functions/#accessing-hasura-session-variables-in-custom-functions" />
      </Flex>
      {currentFunction.configuration?.session_argument ? (
        <Text>{currentFunction.configuration.session_argument}</Text>
      ) : (
        <Flex>{notSetBadge}</Flex>
      )}
    </>
  );
};
