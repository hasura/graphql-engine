import React from 'react';
import Headers, { Header } from '../../../../Common/Headers/Headers';

const editorLabel = 'Headers';
const editorSecText =
  'Headers Hasura will send to the webhook with the POST request.';

type HeaderConfEditorProps = {
  forwardClientHeaders: boolean;
  toggleForwardClientHeaders: (e: React.ChangeEvent<HTMLInputElement>) => void;
  headers: Header[];
  setHeaders: (hs: Header[]) => void;
  disabled?: boolean;
};

const HeaderConfEditor: React.FC<HeaderConfEditorProps> = ({
  forwardClientHeaders,
  toggleForwardClientHeaders,
  headers,
  setHeaders,
  disabled = false,
}) => {
  return (
    <>
      <h2 className="text-lg font-semibold">{editorLabel}</h2>
      <p className="text-sm text-gray-600 mb-sm">{editorSecText}</p>

      <div className="inline-flex items-center mr-sm mb-sm">
        <input
          id="checkbox-1"
          name="checkbox"
          type="checkbox"
          checked={forwardClientHeaders}
          onChange={toggleForwardClientHeaders}
          disabled={disabled}
          className="mr-sm border-gray-400 rounded input-border"
        />
        <label className="pt-sm pl-sm" htmlFor="checkbox-1">
          Forward client headers to webhook
        </label>
      </div>
      <Headers headers={headers} setHeaders={setHeaders} disabled={disabled} />
    </>
  );
};

export default HeaderConfEditor;
