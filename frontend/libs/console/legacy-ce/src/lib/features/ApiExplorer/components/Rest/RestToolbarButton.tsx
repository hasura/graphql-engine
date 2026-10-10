import { useState } from 'react';
import {
  print,
  OperationDefinitionNode,
  OperationTypeNode,
  GraphQLSchema,
} from 'graphql';
import { hasuraToast, DropdownMenu } from '@hasura/shared/ui';
import { ToolbarButton } from '@graphiql/react';
import RestIcon from './RestIcon';
import { setLSItem } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';
import { trackGraphiQlToolbarButtonClick } from '../customAnalyticsEvents';
import { LS_KEYS } from '@hasura/shared/types';

type Props = {
  operations: OperationDefinitionNode[] | null | undefined;
  schema: GraphQLSchema | null | undefined;
};

const errorTitle = 'Unable to create a REST endpoint';

const RestToolbarButton = ({ operations, schema }: Props) => {
  const navigate = useNavigate();
  const [optionsOpen, setOptionsOpen] = useState(false);

  const handleClick = () => {
    trackGraphiQlToolbarButtonClick('REST');
    if (!operations?.length) {
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: 'The input query is empty',
      });
      return;
    }

    if (operations) {
      if (operations.length > 1) {
        setOptionsOpen(!optionsOpen);
        return;
      }

      if (operations.length === 1) {
        onRun(operations[0]);
      }
    }
  };

  const onRun = (operation: OperationDefinitionNode) => {
    if (operation.operation === OperationTypeNode.SUBSCRIPTION) {
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: 'REST endpoint does not support subscription',
      });
    }

    const plainQuery = print(operation);
    setLSItem(LS_KEYS.graphiqlQuery, plainQuery);
    setOptionsOpen(false);
    navigate('/api/rest/create?from=graphiql');
  };

  const _onOptionSelected = (operation: OperationDefinitionNode) => {
    setOptionsOpen(false);
    onRun(operation);
  };

  const validOperations =
    operations
      ?.filter(
        (operation) => operation.operation !== OperationTypeNode.SUBSCRIPTION,
      )
      .map((operation, i) => {
        const opName = operation.name
          ? operation.name.value
          : `<Unnamed ${operation.operation}>`;
        return (
          <DropdownMenu.Item
            key={opName}
            onSelect={() => _onOptionSelected(operation)}
          >
            {opName}
          </DropdownMenu.Item>
        );
      }) ?? [];

  const buttonIcon = (
    <RestIcon className="graphiql-toolbar-icon w-auto! h-auto!" />
  );

  return (
    <>
      {validOperations.length > 1 ? (
        <DropdownMenu.Root items={validOperations}>
          <ToolbarButton label="REST Endpoints">{buttonIcon}</ToolbarButton>
        </DropdownMenu.Root>
      ) : (
        <ToolbarButton
          label="REST Endpoints"
          onClick={handleClick}
          disabled={!schema || !operations?.length}
        >
          {buttonIcon}
        </ToolbarButton>
      )}
    </>
  );
};

export default RestToolbarButton;
