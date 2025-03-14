import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';

import Headers, { Header } from '../../../../Common/Headers/Headers';
import { parseServerHeaders } from '../../../../Common/Headers/utils';

import { EventTrigger, VoidCallback } from '../../types';

type HeaderEditorProps = {
  currentTrigger: EventTrigger;
  headers: Header[];
  setHeaders: (h: Header[]) => void;
  save: (success: VoidCallback, error: VoidCallback) => void;
};

const thStyle =
  'px-md py-sm text-left text-sm font-medium text-gray-600 uppercase tracking-wider';

const HeadersEditor = (props: HeaderEditorProps) => {
  const { setHeaders, headers, save, currentTrigger } = props;

  const existingHeaders = parseServerHeaders(
    currentTrigger.configuration.headers
  );
  const numExistingHeaders = currentTrigger.configuration.headers
    ? currentTrigger.configuration.headers.length
    : 0;

  const reset = () => {
    setHeaders(existingHeaders);
  };

  const collapsed = () => (
    <>
      {numExistingHeaders > 0 ? (
        <div className="overflow-x-auto border border-gray-300 rounded mb-sm">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                <th className={thStyle}>Key</th>
                <th className={thStyle}>Type</th>
                <th className={thStyle}>Value</th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {existingHeaders
                .filter(h => !!h.name)
                .map(header => (
                  <tr className="">
                    <td className="px-md py-sm whitespace-nowrap font-medium">
                      {header.name}
                    </td>
                    <td className="px-md py-sm whitespace-nowrap text-gray-600">
                      {header.type}
                    </td>
                    <td className="px-md py-sm whitespace-nowrap text-gray-600">
                      {header.value}
                    </td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
      ) : (
        <div>No headers</div>
      )}
    </>
  );

  const expanded = () => (
    <div>
      <Headers headers={headers} setHeaders={setHeaders} />
    </div>
  );

  return (
    <div className="mb-lg w-9/12">
      <h2 className="text-lg font-semibold mb-xs flex items-center">Headers</h2>
      <p className="text-sm mb-sm text-gray-600">
        Headers Hasura will send to the webhook with the POST request.
      </p>
      <Editor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        expandCallback={reset}
        property="headers"
        service="modify-trigger"
        saveFunc={save}
        dataTest="edit-header"
      />
    </div>
  );
};

export default HeadersEditor;
