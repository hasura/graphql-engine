import { useState } from 'react';
import QueryAnalyzer from './QueryAnalyzer';
import { print, OperationDefinitionNode } from 'graphql';
import { isValidGraphQLOperation } from '../../utils';
import { getGraphQLQueryPayload } from '@hasura/metadata/api';
import { trackGraphiQlToolbarButtonClick } from '../customAnalyticsEvents';
import { hasuraToast, DropdownMenu } from '@hasura/shared/ui';
import { GraphQLRequestInput, GraphiqlMode } from '@hasura/shared/types';
import { ToolbarButton } from '@graphiql/react';
import { getErrorMessage } from '@hasura/shared/utils';
import AnalyzerIcon from './AnalyzerIcon';

type Props = {
  operations: OperationDefinitionNode[] | null | undefined;
  query: string;
  variables: string;
  headers: Record<string, any>;
  mode: GraphiqlMode;
};

const errorTitle = 'Analyze query error';

const AnalyzeButton = ({
  operations,
  mode,
  query,
  variables,
  headers,
}: Props) => {
  const [optionsOpen, setOptionsOpen] = useState(false);
  const [isAnalysing, setIsAnalysing] = useState(false);

  const [analyseQuery, setAnalyseQuery] = useState<GraphQLRequestInput | null>(
    null,
  );

  const handleAnalyseClick = () => {
    if (!operations?.length) {
      // Don't do anything and return
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: 'No valid query',
      });
    }

    if (operations) {
      if (operations.length > 1) {
        setOptionsOpen(!optionsOpen);
        return;
      }

      if (operations.length === 1) {
        // Handle analyse click
        onRun(operations[0]);
      }
    }

    trackGraphiQlToolbarButtonClick('Analyze');
  };

  const clearAnalyse = () => {
    setIsAnalysing(false);
    setAnalyseQuery(null);
  };

  const onRun = (operation: OperationDefinitionNode) => {
    let jsonVariables: Record<string, any> | null = {};
    try {
      jsonVariables =
        variables && variables.trim() !== '' ? JSON.parse(variables) : null;
    } catch (e) {
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: `Variables are invalid JSON: ${getErrorMessage(e)}`,
      });
      return;
    }

    if (jsonVariables && typeof jsonVariables !== 'object') {
      hasuraToast({
        type: 'error',
        title: errorTitle,
        message: `Variables are not a JSON object.`,
      });
      return;
    }

    const plainQuery = print(operation);
    const q = getGraphQLQueryPayload(plainQuery, jsonVariables);
    if (operation.name?.value) {
      q.operationName = operation.name.value;
    }

    setAnalyseQuery(q);
    setIsAnalysing(true);
    setOptionsOpen(false);
  };

  const _onOptionSelected = (operation: OperationDefinitionNode) => {
    setOptionsOpen(false);
    onRun(operation);
  };

  const validOperations =
    operations?.filter(isValidGraphQLOperation)?.map((operation, i) => {
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

  const buttonIcon = <AnalyzerIcon className="graphiql-toolbar-icon" />;

  return (
    <>
      {validOperations.length > 1 ? (
        <DropdownMenu.Root items={validOperations}>
          <ToolbarButton label="Analyze Query">{buttonIcon}</ToolbarButton>
        </DropdownMenu.Root>
      ) : (
        <ToolbarButton
          label="Analyze Query"
          onClick={handleAnalyseClick}
          disabled={!query}
        >
          {buttonIcon}
        </ToolbarButton>
      )}
      {analyseQuery?.query && isAnalysing && (
        <QueryAnalyzer
          mode={mode}
          query={analyseQuery}
          headers={headers}
          clearAnalyse={clearAnalyse}
        />
      )}
    </>
  );
};

export default AnalyzeButton;
