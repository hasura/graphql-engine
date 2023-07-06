import z from 'zod';
import { Collapsible } from '../../../../../new-components/Collapsible';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Switch } from '../../../../../new-components/Switch';
import { InputField } from '../../../../../new-components/Form';
import { FaShieldAlt } from 'react-icons/fa';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';
import { BooleanCheckbox } from './BooleanCheckbox';
import { useFormContext } from 'react-hook-form';
import { Badge } from '../../../../../new-components/Badge';

export const inputValidationSchema = z.object({
  type: z.enum(['http']),
  enabled: z.boolean(),
  definition: z.object({
    url: z.string().url(),
    forward_client_headers: z.boolean().optional(),
    headers: z
      .array(
        z.object({
          name: z.string(),
          value: z.string(),
          type: z.string(),
        })
      )
      .optional(),
    timeout: z.number().positive().optional(),
  }),
});

export const InputValidation = () => {
  const { setValue, watch } = useFormContext();
  const enabled = watch('enabled');
  return (
    <Collapsible
      triggerChildren={
        <h2 className="text-normal font-semibold flex items-center">
          Input Validation
          <div className="flex items-center">
            <Badge className="mx-2 text-xs" color="blue">
              BETA
            </Badge>
            <IconTooltip message="When enabled, the input data will be validated with provided configuration." />
            {/* TODO: add doc link */}
            {/* <LearnMoreLink href="" /> */}
            <p className="italic text-normal font-normal pl-5">
              {enabled ? `- enabled ` : `-disabled`}
            </p>
          </div>
        </h2>
      }
      chevronClass="text-xs mr-sm stroke-2"
    >
      <div className="w-1/2">
        <p className="pb-4">
          Hook an HTTP endpoint to perform input validations
        </p>
        <div className="flex items-center mb-sm">
          <Switch
            checked={enabled}
            onCheckedChange={() => {
              setValue('enabled', !enabled);
            }}
            data-testid="enableValidation"
          />
          <span className="ml-xs cursor-pointer">Enable Input Validation</span>
        </div>
        {enabled ? (
          <div>
            <InputField
              learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
              tooltipIcon={
                <FaShieldAlt className="h-4 text-muted cursor-pointer" />
              }
              name="definition.url"
              label="Webhook URL"
              placeholder="Webhook URL or {{MY_WEBHOOK_URL}}/handler"
              tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
              description="Note: Provide an URL or use an env var to template the handler URL if you have different URLs for multiple environments."
            />
            <div className="mb-xs">
              <label className="flex items-center text-gray-600 font-semibold mb-xs">
                Header
                <IconTooltip message="Configure headers for the request to the webhook" />
              </label>
              <div className="inline-flex">
                <BooleanCheckbox
                  name="definition.forward_client_headers"
                  text="Forward client headers to webhook"
                />
              </div>
              <RequestHeadersSelector
                name="definition.headers"
                addButtonText="Add Additional Headers"
              />
            </div>
            <InputField
              type="number"
              name="definition.timeout"
              label="Timeout"
              placeholder="10 (default)"
              tooltip="Configure timeout for input validation. Default is 10 seconds"
              appendLabel="seconds"
              className="w-1/2"
            />
          </div>
        ) : null}
      </div>
    </Collapsible>
  );
};
