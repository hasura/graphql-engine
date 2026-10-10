import z from 'zod';
import {
  Collapsible,
  IconTooltip,
  InputField,
  CheckboxField,
  SwitchField,
  Text,
  RequestHeadersSelector,
  requestHeadersSelectorSchema,
} from '@hasura/shared/ui';
import { FaShieldAlt } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { Em, Flex, Strong } from '@radix-ui/themes';

export const inputValidationEnabledSchema = z.object({
  enabled: z.literal(true),
  type: z.enum(['http']),
  definition: z.object({
    url: z.union([z.string().url(), z.string().regex(/{{.*}}\/.*/)]),
    forward_client_headers: z.boolean().optional(),
    headers: requestHeadersSelectorSchema.optional(),
    timeout: z.union([z.number().int().positive().min(1), z.nan()]).optional(),
  }),
});

export const inputValidationSchema = z.discriminatedUnion('enabled', [
  z.object({
    enabled: z.literal(false),
  }),
  inputValidationEnabledSchema,
]);

export type TablePermissionInputValidationSchema = z.infer<
  typeof inputValidationSchema
>;

export const InputValidation = ({ formFieldsNamePrefix = '' }) => {
  const { watch, register } = useFormContext();

  const enabled = watch(formFieldsNamePrefix + 'enabled', false);

  return (
    <Collapsible
      triggerChildren={
        <Flex align="center" gap="2">
          <Text>
            <Strong>Input Validation</Strong>
          </Text>
          <IconTooltip message="When enabled, the input data will be validated with provided configuration." />
          {/* TODO: add doc link */}
          {/* <LearnMoreLink href="" /> */}
          <Text size="1">
            <Em>{enabled ? `- enabled ` : `- disabled`}</Em>
          </Text>
        </Flex>
      }
    >
      <div>
        <Text>Hook an HTTP endpoint to perform input validations</Text>
        <Flex align="center" className="my-2">
          <SwitchField
            name={formFieldsNamePrefix + 'enabled'}
            label="Enable Input Validation"
            data-testid="enableValidation"
          />
        </Flex>
        {enabled ? (
          <div>
            <input
              type="hidden"
              value="http"
              {...register(formFieldsNamePrefix + 'type')}
            />
            <InputField
              learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
              tooltipIcon={
                <Flex align="center" gap="2">
                  <Text color="red">*</Text>
                  <FaShieldAlt />
                </Flex>
              }
              name={formFieldsNamePrefix + 'definition.url'}
              label="Webhook URL"
              fieldProps={{
                placeholder: 'Webhook URL or {{MY_WEBHOOK_URL}}/handler',
              }}
              tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
              description="Note: Provide an URL or use an env var to template the handler URL if you have different URLs for multiple environments."
            />
            <div className="mb-4">
              <CheckboxField
                label="Header"
                tooltip="Configure headers for the request to the webhook"
                name={
                  formFieldsNamePrefix + 'definition.forward_client_headers'
                }
              >
                Forward client headers to webhook
              </CheckboxField>
              <RequestHeadersSelector
                name={formFieldsNamePrefix + 'definition.headers'}
                addButtonText="Add Additional Headers"
              />
            </div>
            <div className="sm:w-1/2">
              <InputField
                fieldProps={{
                  type: 'number',
                  placeholder: '10 (default)',
                  appendLabel: 'seconds',
                }}
                name={formFieldsNamePrefix + 'definition.timeout'}
                label="Timeout"
                tooltip="Configure timeout for input validation. Default is 10 seconds"
              />
            </div>
          </div>
        ) : null}
      </div>
    </Collapsible>
  );
};
