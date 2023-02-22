import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '../../../../../hooks/useDebounceEffect';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { inputStyles } from '../../constants';
import { FaShieldAlt } from 'react-icons/fa';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';

const editorLabel = 'Webhook (HTTP/S) Handler';

type HandlerEditorProps = {
  value: string;
  onChange: (v: string) => void;
  disabled: boolean;
};

const HandlerEditor: React.FC<HandlerEditorProps> = ({
  value,
  onChange,
  disabled = false,
}) => {
  const [localValue, setLocalValue] = useState<string>(value);

  useEffect(() => {
    setLocalValue(value);
  }, [value]);

  useDebouncedEffect(
    () => {
      onChange(localValue);
    },
    1000,
    [localValue]
  );

  return (
    <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
      <div className="mb-lg w-6/12">
        <h2 className="text-lg font-semibold mb-xs flex items-center">
          {editorLabel}
          <span className="text-red-700 ml-xs">*</span>
          <IconTooltip
            message="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
            icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
          />
          <LearnMoreLink href="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl" />
        </h2>
        <p className="text-sm text-gray-600 mb-sm">
          Note: Provide an URL or use an env var to template the handler URL if
          you have different URLs for multiple environments.
        </p>
        <input
          disabled={disabled}
          type="text"
          name="handler"
          value={localValue}
          onChange={e => setLocalValue(e.target.value)}
          placeholder="http://custom-logic.com/api or {{ACTION_BASE_URL}}/handler"
          className={inputStyles}
          data-test="action-create-handler-input"
        />
      </div>
    </Analytics>
  );
};

export default HandlerEditor;
