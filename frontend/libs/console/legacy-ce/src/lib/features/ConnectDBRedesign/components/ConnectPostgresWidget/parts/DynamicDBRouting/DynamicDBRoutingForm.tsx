import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaExclamationCircle, FaPlusCircle } from 'react-icons/fa';
import z from 'zod';
import { Badge } from '../../../../../../new-components/Badge';
import { Button } from '../../../../../../new-components/Button';
import { CardedTable } from '../../../../../../new-components/CardedTable';
import { CardRadioGroup } from '../../../../../../new-components/CardRadioGroup';
import { schema as postgresSchema } from '../../schema';
import { CodeEditorField } from '../../../../../../new-components/Form';
import { LearnMoreLink } from '../../../../../../new-components/LearnMoreLink';
import { IconTooltip } from '../../../../../../new-components/Tooltip';

const editorOptions = {
  minLines: 33,
  maxLines: 33,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

const templates = {
  disabled: {
    value: 'disabled',
    title: 'Disabled',
    body: 'Use default Hasura connection routing.',
    template: '',
    isSelected: (connectionTemplate?: string | null) => !connectionTemplate,
  },
  tenancy: {
    value: 'tenancy',
    title: 'Database Tenancy',
    body: 'Tenancy template using a x-hasura-tenant variable to connect to a named database.',
    template: `{{ if ($.request.session.x-hasura-tenant-id == "my_tenant_1")}}
    {{$.connection_set.my_tenant_1_connection}}
{{ elif ($.request.session.x-hasura-tenant-id == "my_tenant_2")}}
    {{$.connection_set.my_tenant_2_connection}}
{{ else }}
    {{$.default}}
{{ end }}`,
    isSelected: (connectionTemplate?: string | null) =>
      connectionTemplate?.includes('x-hasura-tenant-id'),
  },
  'no-stale-reads': {
    value: 'no-stale-reads',
    title: 'Read Replicas - No Stale Reads',
    body: 'No stale reads template using the primary for reads on mutations and replicas for all other reads.',
    template: `{{ if (($.request.query.operation_type == "query") 
|| ($.request.query.operation_type == "subscription")) 
&& ($.request.headers.x-query-read-no-stale == "true") }}
    {{$.primary}}
{{ else }}
    {{$.default}}
{{ end }}`,

    isSelected: (connectionTemplate?: string | null) =>
      connectionTemplate?.includes('x-query-read-no-stale'),
  },
  sharding: {
    value: 'sharding',
    title: 'Different credentials',
    body: 'Route specific queries to specific databases in a distributed database system model.',
    template: `{{ if ($.request.session.x-hasura-role == "manager")}}
    {{$.connection_set.manager_connection}}
{{ elif ($.request.session.x-hasura-role == "employee")}}
    {{$.connection_set.employee_connection}}
{{ else }}
    {{$.default}}
{{ end }}`,
    isSelected: (connectionTemplate?: string | null) =>
      connectionTemplate?.includes('x-hasura-role'),
  },
  custom: {
    value: 'custom',
    title: 'Custom Template',
    body: 'Write a custom connection template using Kriti templating.',
    template: `{{ if ()}}
    {{$.}}
{{ elif ()}}
    {{$.}}
{{ else }}
    {{$.default}}
{{ end }}`,
    isSelected: (connectionTemplate?: string | null) => !!connectionTemplate,
  },
};

interface DynamicDBRoutingFormProps {
  connectionSetMembers: z.infer<typeof postgresSchema>[];
  onAddConnection: () => void;
  onRemoveConnection: (name: string) => void;
  onEditConnection: (name: string) => void;
  isLoading: boolean;
  connectionTemplate?: string | null;
}

export const DynamicDBRoutingForm = (props: DynamicDBRoutingFormProps) => {
  const {
    connectionSetMembers,
    onAddConnection,
    onRemoveConnection,
    onEditConnection,
    isLoading,
    connectionTemplate,
  } = props;
  const { setValue } = useFormContext();
  const [template, setTemplate] = React.useState<keyof typeof templates>(
    (Object.entries(templates).find(([_, template]) =>
      template.isSelected(connectionTemplate)
    )?.[0] as keyof typeof templates) || 'disabled'
  );

  return (
    <div>
      <div>
        <div className="mb-2">
          {template !== 'disabled' && (
            <div
              className={`flex items-center rounded bg-gray-200 border border-gray-300 py-sm px-sm mb-md`}
            >
              <FaExclamationCircle className="fill-current self-start h-md text-muted" />
              <div className="ml-xs">
                <strong>Dynamic Database Routing Precedence</strong>
                <p>
                  {' '}
                  Dynamic database routing takes precedence over read replicas.
                  You may use both read replica routing and default database
                  routing in your connection template.
                </p>
              </div>
            </div>
          )}
          <div className="block flex items-center text-gray-600 font-semibold">
            <label htmlFor="connection_template" className="font-semibold">
              Connection Template
            </label>
            <IconTooltip message="Connection templates to route GraphQL requests based on different request parameters such as session variables, headers and tenant IDs." />
            <LearnMoreLink
              href="https://hasura.io/docs/latest/databases/connect-db/dynamic-db-connection/#connection-template"
              className="font-normal"
            />
          </div>
          <div className="text-muted">
            Database connection template to define dynamic connection routing.
          </div>
        </div>
        <div className="grid grid-cols-2 gap-4">
          <CardRadioGroup
            value={template}
            orientation="vertical"
            onChange={value => {
              setTemplate(value as keyof typeof templates);
              setValue(
                'connection_template',
                templates[value as keyof typeof templates]?.template
              );
            }}
            items={Object.values(templates).map(template => ({
              value: template.value,
              title: template.title,
              body: template.body,
            }))}
          />
          <div data-testid="template-editor">
            <CodeEditorField
              disabled={template === 'disabled'}
              noErrorPlaceholder
              name="connection_template"
              editorOptions={editorOptions}
            />
          </div>
        </div>
      </div>
      <div className="mb-2 mt-8 flex justify-between items-end">
        <div>
          <div className="block flex items-center text-gray-600 font-semibold">
            <label htmlFor="template" className="font-semibold">
              Available Connections for Templating
            </label>
            <IconTooltip message="Available database connections which can be referenced in your dynamic connection template." />
            <LearnMoreLink
              href="https://hasura.io/docs/latest/databases/connect-db/dynamic-db-connection/#connection-set"
              text="(Learn More)"
              className="font-normal"
            />
          </div>
          <div className="text-muted">
            Available connections which can be referenced in your dynamic
            connection template.{' '}
          </div>
        </div>

        <Button
          onClick={onAddConnection}
          icon={<FaPlusCircle />}
          disabled={isLoading}
        >
          Add Connection
        </Button>
      </div>
      <div>
        <CardedTable
          showActionCell
          columns={['Connection']}
          data={[
            [
              '{{$.default}}',
              <Badge color="light-gray">Default Routing Behavior</Badge>,
            ],
            [
              '{{$.primary}}',
              <Badge color="light-gray">The Database Primary</Badge>,
            ],
            [
              '{{$.read_replicas}}',
              <Badge color="light-gray">Read Replica Routing</Badge>,
            ],
            ...connectionSetMembers.map(connection => [
              `{{$.${connection.name}}}`,
              <>
                <Button
                  disabled={isLoading}
                  className="mr-2"
                  size="sm"
                  onClick={() => onEditConnection(connection.name)}
                >
                  Edit Connection
                </Button>
                <Button
                  disabled={isLoading}
                  mode="destructive"
                  size="sm"
                  onClick={() => onRemoveConnection(connection.name)}
                >
                  Remove
                </Button>
              </>,
            ]),
          ]}
        />
      </div>
      <div className="flex justify-end">
        <Button type="submit" mode="primary">
          Update Database Connection
        </Button>
      </div>
    </div>
  );
};
