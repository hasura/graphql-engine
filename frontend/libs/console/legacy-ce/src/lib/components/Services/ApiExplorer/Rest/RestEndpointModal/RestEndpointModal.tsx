import React from 'react';
import { Dialog } from '../../../../../new-components/Dialog';
import { Checkbox } from '../../../../../new-components/Form';
import { useCreateRestEndpoints } from '../../../../../features/RestEndpoints/hooks/useCreateRestEndpoints';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { EndpointType } from '../../../../../features/RestEndpoints/hooks/useRestEndpointDefinitions';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { Badge, BadgeColor } from '../../../../../new-components/Badge';
import { Link, browserHistory } from 'react-router';
import { FaExclamation, FaExternalLinkAlt } from 'react-icons/fa';

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
}

export const RestEndpointModal = (props: RestEndpointModalProps) => {
  const { onClose, tableName } = props;
  const { createRestEndpoints, endpointDefinitions, isLoading } =
    useCreateRestEndpoints();

  const tableEndpointDefinitions = endpointDefinitions?.[tableName] ?? {};

  const [selectedMethods, setSelectedMethods] = React.useState<EndpointType[]>(
    []
  );

  const filteredEndpoints = React.useMemo(
    () =>
      ENDPOINTS.filter(
        method => !!tableEndpointDefinitions[method.value as EndpointType]
      ),
    [tableEndpointDefinitions]
  );

  return (
    <Dialog
      hasBackdrop
      title="Auto-Create REST Endpoints"
      onClose={onClose}
      description="One-click to create REST endpoint from selected table"
      footer={
        <Dialog.Footer
          onSubmitAnalyticsName="data-tab-rest-endpoints-modal-create"
          onCancelAnalyticsName="data-tab-rest-endpoints-modal-cancel"
          callToAction="Create"
          isLoading={isLoading}
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
                    ', '
                  )}`,
                });
                onClose();
                const createdEndpoints = selectedMethods.map(
                  method =>
                    endpointDefinitions?.[tableName]?.[method]?.query?.name
                );

                browserHistory.push(
                  `/api/rest/list?highlight=${createdEndpoints.join(',')}`
                );
              },
              onError: error => {
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
      <div className="p-4 flex flex-col gap-4">
        {filteredEndpoints.length > 0 && (
          <CardedTable
            columns={[
              <Checkbox
                checked={selectedMethods.length === ENDPOINTS.length}
                onCheckedChange={checked => {
                  if (checked) {
                    setSelectedMethods(
                      ENDPOINTS.map(endpoint => endpoint.value)
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
            data={filteredEndpoints.map(method => {
              const endpointDefinition =
                tableEndpointDefinitions[method.value as EndpointType];

              return [
                <Checkbox
                  checked={selectedMethods.includes(
                    method.value as EndpointType
                  )}
                  disabled={endpointDefinition?.exists}
                  onCheckedChange={checked => {
                    if (checked) {
                      setSelectedMethods([
                        ...selectedMethods,
                        method.value as EndpointType,
                      ]);
                    } else {
                      setSelectedMethods(
                        selectedMethods.filter(
                          selectedMethod => selectedMethod !== method.value
                        )
                      );
                    }
                  }}
                />,
                <div>
                  {endpointDefinition?.exists ? (
                    <Link
                      to={{
                        pathname: `/api/rest/details/${endpointDefinition.restEndpoint?.name}`,
                        state: {
                          ...endpointDefinition.restEndpoint,
                          currentQuery: endpointDefinition.query.query,
                        },
                      }}
                    >
                      {method.label}{' '}
                      <FaExternalLinkAlt className="relative ml-1 -top-0.5" />
                    </Link>
                  ) : (
                    method.label
                  )}
                </div>,
                <Badge color={method.color}>
                  {endpointDefinition?.restEndpoint?.methods?.join(', ')}
                </Badge>,
                <div>/{endpointDefinition?.restEndpoint?.url ?? 'N/A'}</div>,
              ];
            })}
          />
        )}
        {filteredEndpoints.length === 0 && (
          <IndicatorCard
            showIcon
            children={
              <div>
                No REST endpoints can be created for this table
                <LearnMoreLink href="https://hasura.io/docs/latest/restified/overview" />
              </div>
            }
            status="negative"
            customIcon={FaExclamation}
          />
        )}
        {filteredEndpoints.length > 0 && (
          <IndicatorCard
            showIcon
            children={
              <div>
                Creating REST Endpoints will add metadata entries to your Hasura
                project
                <LearnMoreLink href="https://hasura.io/docs/latest/restified/overview" />
              </div>
            }
            status="info"
            customIcon={FaExclamation}
          />
        )}
      </div>
    </Dialog>
  );
};
