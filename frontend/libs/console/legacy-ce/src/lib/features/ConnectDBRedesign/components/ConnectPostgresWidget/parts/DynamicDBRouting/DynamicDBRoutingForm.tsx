import React from 'react';
import { Flex } from '@radix-ui/themes';
import { useFormContext } from 'react-hook-form';
import { FaExclamationCircle, FaPlay, FaPlusCircle } from 'react-icons/fa';
import z from 'zod';
import {
  Badge,
  Button,
  CardedTable,
  CodeEditorField,
  LearnMoreLink,
  IconTooltip,
  RadioCardGroup,
  Text,
  Card,
  Link,
} from '@hasura/shared/ui';

import { schema as postgresSchema } from '../../schema';

import { Analytics, trackCustomEvent } from '@hasura/shared/analytics';

const editorOptions = {
  minLines: 34,
  maxLines: 34,
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
    body: 'Using x-hasura-tenant variable to route queries to different tenant databases.',
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
    body: 'Routing reads to primary instead of read-replicas based on a header configuration.',
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
    title: 'Multi-user Database Credentials',
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
  onOpenValidate: () => void;
}

export const DynamicDBRoutingForm = (props: DynamicDBRoutingFormProps) => {
  const {
    connectionSetMembers,
    onAddConnection,
    onRemoveConnection,
    onEditConnection,
    isLoading,
    connectionTemplate,
    onOpenValidate,
  } = props;
  const { setValue, watch } = useFormContext();
  const [template, setTemplate] = React.useState<keyof typeof templates>(
    (Object.entries(templates).find(([_, template]) =>
      template.isSelected(connectionTemplate),
    )?.[0] as keyof typeof templates) || 'disabled',
  );

  const localConnectionTemplate = watch('connection_template');

  const [localTemplate, setLocalTemplate] = React.useState<
    Record<string, string | null | undefined>
  >(
    Object.fromEntries(
      Object.entries(templates).map(([key, template]) => [
        key,
        template.isSelected(connectionTemplate)
          ? connectionTemplate
          : template.template,
      ]),
    ),
  );

  return (
    <div>
      <div>
        <div className="mb-2">
          {template !== 'disabled' && (
            <Card className={'mb-4'}>
              <Flex align="center" gap="2">
                <FaExclamationCircle className="fill-current self-start h-6 text-muted" />
                <div className="max-w-2xl">
                  <Text as="p" weight="bold">
                    Dynamic Routing Precedence
                  </Text>
                  <Text>
                    {' '}
                    Dynamic routing takes precedence over read replicas. You may
                    use both read replica routing and default database routing
                    in your connection template.
                  </Text>
                </div>
                <Button mode="default" asChild>
                  <Link
                    href="https://hasura.io/docs/latest/databases/database-config/dynamic-db-connection/#setting-up-connection-set-and-connection-template"
                    target="__blank"
                  >
                    Learn More
                  </Link>
                </Button>
              </Flex>
            </Card>
          )}
          <Flex align="center" gap="2">
            <Text as="label" htmlFor="connection_template" weight="medium">
              Connection Template
            </Text>
            <IconTooltip message="Connection templates to route GraphQL requests based on different request parameters such as session variables, headers and tenant IDs." />
            <LearnMoreLink
              href="https://hasura.io/docs/latest/databases/database-config/dynamic-db-connection/#connection-template"
              className="font-normal"
            />
          </Flex>
          <Text as="p">
            Database connection template to define dynamic connection routing.
          </Text>
        </div>
        <div className="grid grid-cols-2 gap-4">
          <RadioCardGroup
            value={template}
            orientation="vertical"
            onChange={(value) => {
              setLocalTemplate({
                ...localTemplate,
                [template]: localConnectionTemplate,
              });
              setTemplate(value as keyof typeof templates);
              setValue(
                'connection_template',
                localTemplate[value as keyof typeof templates],
              );
              trackCustomEvent(
                {
                  location: 'DynamicDBRouting',
                  action: 'change',
                  object: 'defaultTemplate',
                },
                {
                  data: {
                    temlate: value,
                  },
                },
              );
            }}
            options={Object.values(templates).map((template) => ({
              value: template.value,
              label: (
                <div>
                  <Text as="div" weight="bold">
                    {template.title}
                  </Text>
                  <Text as="div">{template.body}</Text>
                </div>
              ),
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
        <Flex justify="end" className="mt-4" gap="2">
          <Analytics
            name="data-tab-dynamic-db-routing-validate-connection-template"
            passHtmlAttributesToChildren
          >
            <Button
              size="1"
              onClick={onOpenValidate}
              disabled={template === 'disabled'}
              leftIcon={FaPlay}
              mode="default"
            >
              Validate
            </Button>
          </Analytics>
          <Analytics
            name="data-tab-dynamic-db-routing-update-connection-template"
            passHtmlAttributesToChildren
          >
            <Button
              type="submit"
              mode="primary"
              size="1"
              disabled={
                isLoading || localConnectionTemplate === connectionTemplate
              }
            >
              Update Connection Template
            </Button>
          </Analytics>
        </Flex>
      </div>
      <Flex justify="between" align="end" className="mb-2 mt-8">
        <div>
          <Flex align="center" gap="2">
            <Text as="label" weight="medium" htmlFor="template">
              Available Connections for Templating
            </Text>
            <IconTooltip message="Available database connections which can be referenced in your dynamic connection template." />
            <LearnMoreLink
              href="https://hasura.io/docs/latest/databases/database-config/dynamic-db-connection/#connection-set"
              text="(Learn More)"
            />
          </Flex>
          <Text as="p">
            Available connections which can be referenced in your dynamic
            connection template.
          </Text>
        </div>
        <Analytics
          name="data-tab-dynamic-db-routing-add-connection"
          passHtmlAttributesToChildren
        >
          <Button
            mode="default"
            size="1"
            onClick={onAddConnection}
            leftIcon={FaPlusCircle}
            disabled={isLoading}
          >
            Add Connection
          </Button>
        </Analytics>
      </Flex>
      <div>
        <CardedTable
          columns={['Connection', '']}
          data={[
            [
              '{{$.default}}',
              <Badge key="default" color="gray">
                Default Routing Behavior
              </Badge>,
            ],
            [
              '{{$.primary}}',
              <Badge key="primary" color="gray">
                The Database Primary
              </Badge>,
            ],
            [
              '{{$.read_replicas}}',
              <Badge key="read_replicas" color="gray">
                Read Replica Routing
              </Badge>,
            ],
            ...connectionSetMembers.map((connection) => [
              `{{$.connection_set.${connection.name}}}`,
              <>
                <Analytics
                  name="data-tab-dynamic-db-routing-edit-connection"
                  passHtmlAttributesToChildren
                >
                  <Button
                    disabled={isLoading}
                    className="mr-2"
                    size="sm"
                    onClick={() => onEditConnection(connection.name)}
                  >
                    Edit Connection
                  </Button>
                </Analytics>
                <Analytics
                  name="data-tab-dynamic-db-routing-remove-connection"
                  passHtmlAttributesToChildren
                >
                  <Button
                    disabled={isLoading}
                    mode="destructive"
                    size="sm"
                    onClick={() => onRemoveConnection(connection.name)}
                  >
                    Remove
                  </Button>
                </Analytics>
              </>,
            ]),
          ]}
        />
      </div>
    </div>
  );
};
