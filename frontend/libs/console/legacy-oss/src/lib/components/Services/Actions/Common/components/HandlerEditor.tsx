import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '@/hooks/useDebounceEffect';
import { inputStyles } from '../../constants';

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
    <div className="mb-lg w-4/12">
      <h2 className="text-lg font-semibold mb-xs flex items-center">
        {editorLabel}
        <span className="text-red-700 ml-xs mr-sm">*</span>
      </h2>
      <input
        disabled={disabled}
        type="text"
        name="handler"
        value={localValue}
        onChange={e => setLocalValue(e.target.value)}
        placeholder="http://custom-logic.com/api"
        className={inputStyles}
        data-test="action-create-handler-input"
      />
      <p className="text-sm text-gray-600">
        Note: You can use an env var to template the handler URL if you have
        different URLs for multiple environments.
        <br /> e.g. {'{{ACTION_BASE_URL}}/handler'}
      </p>
    </div>
  );
};

export default HandlerEditor;
