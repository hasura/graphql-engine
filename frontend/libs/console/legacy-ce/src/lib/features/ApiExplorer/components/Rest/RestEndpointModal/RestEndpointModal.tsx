import React from 'react';
import {
  hasuraToast,
  Dialog,
  Checkbox,
  IndicatorCard,
  LearnMoreLink,
  CardedTable,
  Badge,
  BadgeColor,
  DialogFooter,
  RelativeLink,
} from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { FaExclamation, FaExternalLinkAlt } from 'react-icons/fa';
import { Table } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import { EndpointType, useCreateRestEndpoints } from '@hasura/metadata/api';

const ENDPOINTS: {
  value: EndpointType;
  label: string;
  color: BadgeColor;
}[] = [
  { value: 'READ', label: 'READ', color: 'indigo' },
  { value: 'READ_ALL', label: 'READ ALL', color: 'indigo' },
  { value: 'CREATE', label: 'CREATE', color: 'yellow' },
  { value: 'UPDATE', label: 'UPDATE', color: 'yellow' },
  { value: 'DELETE', label: 'DELETE', color: 'red' },
];
export interface RestEndpointModalProps {
  onClose: () => void;
  tableName: string;
  dataSourceName: string;
  table: Table;
}

export const RestEndpointModal = (props: RestEndpointModalProps) => {
  const { onClose, tableName, dataSourceName } = props;
  const navigate = useNavigate();
  const { createRestEndpoints, endpointDefinitions, isPending } =
    useCreateRestEndpoints({
      dataSourceName: dataSourceName,
      table: props.table,
    });

  const tableEndpointDefinitions = endpointDefinitions?.[tableName] ?? {};

  const [selectedMethods, setSelectedMethods] = React.useState<EndpointType[]>(
    [],
  );

  const filteredEndpoints = React.useMemo(
    () =>
      ENDPOINTS.filter(
        (method) => !!tableEndpointDefinitions[method.value as EndpointType],
      ),
    [tableEndpointDefinitions],
  );

  return (
    <Dialog
      title="Auto-Create REST Endpoints"
      onClose={onClose}
      description="One-click to create REST endpoint from selected table"
      footer={
        <DialogFooter
          onSubmitAnalyticsName="data-tab-rest-endpoints-modal-create"
          onCancelAnalyticsName="data-tab-rest-endpoints-modal-cancel"
          callToAction="Create"
          isLoading={isPending}
          callToDeny="Cancel"
          onClose={onClose}
          disabled={selectedMethods.length === 0}
          onSubmit={() => {
            createRestEndpoints(tableName, selectedMethods, {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Successfully generated rest endpoints',
                  message: `Successfully generated rest endpoints for ${tableName}: ${selectedMethods.join(
                    ', ',
                  )}`,
                });
                onClose();
                const createdEndpoints = selectedMethods.map(
                  (method) =>
                    endpointDefinitions?.[tableName]?.[method]?.query?.name,
                );

                navigate(
                  `/api/rest/list?highlight=${createdEndpoints.join(',')}`,
                );
              },
              onError: (error) => {
                hasuraToast({
                  type: 'error',
                  title: 'Failed to generate endpoints',
                  message: error.message,
                });
              },
            });
          }}
        />
      }
    >
      <Flex direction="column" gap="4" className="p-4">
        {filteredEndpoints.length > 0 && (
          <CardedTable
            columns={[
              <Checkbox
                key="select-all"
                value={selectedMethods.length === ENDPOINTS.length}
                onChange={(checked) => {
                  if (checked) {
                    setSelectedMethods(
                      ENDPOINTS.map((endpoint) => endpoint.value),
                    );
                  } else {
                    setSelectedMethods([]);
                  }
                }}
              />,
              'OPERATION',
              'METHOD',
              'PATH',
            ]}
            data={filteredEndpoints.map((method) => {
              const endpointDefinition =
                tableEndpointDefinitions[method.value as EndpointType];

              return [
                <Checkbox
                  key={`checkbox-${method.value}`}
                  value={selectedMethods.includes(method.value as EndpointType)}
                  disabled={endpointDefinition?.exists}
                  onChange={(checked) => {
                    if (checked) {
                      setSelectedMethods([
                        ...selectedMethods,
                        method.value as EndpointType,
                      ]);
                    } else {
                      setSelectedMethods(
                        selectedMethods.filter(
                          (selectedMethod) => selectedMethod !== method.value,
                        ),
                      );
                    }
                  }}
                />,
                <div key={`operation-${method.value}`}>
                  {endpointDefinition?.exists ? (
                    <RelativeLink
                      to={`/api/rest/details/${endpointDefinition.restEndpoint?.name}`}
                      state={{
                        ...endpointDefinition.restEndpoint,
                        currentQuery: endpointDefinition.query.query,
                      }}
                    >
                      {method.label}{' '}
                      <FaExternalLinkAlt className="relative ml-1 -top-0.5" />
                    </RelativeLink>
                  ) : (
                    method.label
                  )}
                </div>,
                <Badge key={`method-${method.value}`} color={method.color}>
                  {endpointDefinition?.restEndpoint?.methods?.join(', ')}
                </Badge>,
                <div key={`path-${method.value}`}>
                  /{endpointDefinition?.restEndpoint?.url ?? 'N/A'}
                </div>,
              ];
            })}
          />
        )}
        {filteredEndpoints.length === 0 && (
          <IndicatorCard showIcon status="negative" customIcon={FaExclamation}>
            <div>
              No REST endpoints can be created for this table
              <LearnMoreLink href="https://hasura.io/docs/latest/restified/overview" />
            </div>
          </IndicatorCard>
        )}
        {filteredEndpoints.length > 0 && (
          <IndicatorCard showIcon status="info" customIcon={FaExclamation}>
            <div>
              Creating REST Endpoints will add metadata entries to your Hasura
              project
              <LearnMoreLink href="https://hasura.io/docs/latest/restified/overview" />
            </div>
          </IndicatorCard>
        )}
      </Flex>
    </Dialog>
  );
};
