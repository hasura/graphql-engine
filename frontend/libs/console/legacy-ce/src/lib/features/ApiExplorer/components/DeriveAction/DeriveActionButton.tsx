import { useState } from 'react';
import {
  OperationDefinitionNode,
  OperationTypeNode,
  GraphQLSchema,
} from 'graphql';
import {
  hasuraToast,
  DropdownMenu,
  useHasuraAlert,
  showErrorNotification,
} from '@hasura/shared/ui';
import { ToolbarButton } from '@graphiql/react';
import { CiShare1 } from 'react-icons/ci';
import { useNavigate } from 'react-router';
import { trackGraphiQlToolbarButtonClick } from '../customAnalyticsEvents';
import deriveAction from './utils';
import { dataRoutes } from '@hasura/shared/utils';
import {
  getActionDefinitionSdl,
  getTypesSdl,
} from '../../../../shared/utils/sdlUtils';

type Props = {
  operations: OperationDefinitionNode[] | null | undefined;
  schema: GraphQLSchema | null | undefined;
};

const errorTitle = 'Unable to derive mutation';
const label = 'Derive action for the given mutation';

const DeriveActionButton = ({ operations, schema }: Props) => {
  const navigate = useNavigate();
  const { hasuraConfirm } = useHasuraAlert();
  const [optionsOpen, setOptionsOpen] = useState(false);

  const onRun = (operation: OperationDefinitionNode) => {
    if (!schema) return;

    let derivedOperationMetadata: ReturnType<typeof deriveAction>;

    try {
      derivedOperationMetadata = deriveAction(operation, schema);
    } catch (e) {
      showErrorNotification({
        title: errorTitle,
        error: e,
      });

      return;
    }

    const {
      action,
      types,
      variables: derivedVariables,
    } = derivedOperationMetadata;

    const navigateToCreateAction = () => {
      const typesSdl = getTypesSdl(types);

      const actionsSdl = getActionDefinitionSdl(
        action.name,
        action.type,
        action.arguments,
        action.output_type,
      );
      navigate({
        pathname: dataRoutes.createAction,
        search: `?action_sdl=${encodeURIComponent(
          actionsSdl,
        )}&types_sdl=${encodeURIComponent(typesSdl)}`,
      });
    };

    if (derivedVariables && !derivedVariables.length) {
      hasuraConfirm({
        title: 'Derive action',
        message: `Looks like your ${action.type} does not have variables. This means that the derived action will have no arguments.`,
        confirmText: 'Continue',
        onClose: ({ confirmed }) => {
          if (confirmed) {
            navigateToCreateAction();
          }
        },
      });
      return;
    }

    navigateToCreateAction();
  };

  const handleClick = () => {
    trackGraphiQlToolbarButtonClick('Derive action');

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

  const buttonIcon = <CiShare1 className="graphiql-toolbar-icon" />;

  return (
    <>
      {validOperations.length > 1 ? (
        <DropdownMenu.Root items={validOperations}>
          <ToolbarButton label={label}>{buttonIcon}</ToolbarButton>
        </DropdownMenu.Root>
      ) : (
        <ToolbarButton
          label={label}
          onClick={handleClick}
          disabled={!schema || !operations?.length}
        >
          {buttonIcon}
        </ToolbarButton>
      )}
    </>
  );
};

export default DeriveActionButton;
