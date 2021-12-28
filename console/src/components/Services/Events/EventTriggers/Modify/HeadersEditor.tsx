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
  styles: Record<string, string>;
};

const thStyle =
  'px-md py-sm text-left text-sm font-medium text-gray-600 uppercase tracking-wider';

const HeadersEditor = (props: HeaderEditorProps) => {
  const { setHeaders, headers, styles, save, currentTrigger } = props;

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
                <th scope="col" className={thStyle}>
                  Key
                </th>
                <th scope="col" className={thStyle}>
                  Type
                </th>
                <th scope="col" className={thStyle}>
                  Value
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {existingHeaders
                .filter(h => !!h.name)
                .map(header => (
                  <tr className="">
                    <td className="px-3 py-3 whitespace-nowrap font-medium">
                      {header.name}
                    </td>
                    <td className="px-3 py-3 whitespace-nowrap text-gray-600">
                      {header.type}
                    </td>
                    <td className="px-3 py-3 whitespace-nowrap text-gray-600">
                      {header.value}
                    </td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
      ) : (
        <div className={styles.modifyProperty}>No headers</div>
      )}
    </>
  );

  const expanded = () => (
    <div className={styles.modifyOpsPadLeft}>
      <Headers headers={headers} setHeaders={setHeaders} />
    </div>
  );

  return (
    <div className="mb-lg w-6/12">
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
        styles={styles}
      />
    </div>
  );
};

export default HeadersEditor;
