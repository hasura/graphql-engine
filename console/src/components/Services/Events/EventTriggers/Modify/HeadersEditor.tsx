import React from 'react';
import AceEditor from 'react-ace';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from '../../../../Common/Tooltip/Tooltip';

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
    <div>
      {numExistingHeaders > 0 ? (
        <div className={styles.modifyHeaders}>
          <AceEditor
            mode="json"
            theme="github"
            name="headers"
            value={JSON.stringify(
              existingHeaders.filter(h => !!h.name),
              null,
              4
            )}
            minLines={4}
            maxLines={100}
            width="100%"
            showPrintMargin={false}
            showGutter={false}
            readOnly
          />
        </div>
      ) : (
        <div className={styles.modifyProperty}>No headers</div>
      )}
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyOpsPadLeft}>
      <Headers headers={headers} setHeaders={setHeaders} />
    </div>
  );

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <h4 className={styles.modifySectionHeading}>
          Headers{' '}
          <Tooltip message="Edit headers to be sent along with the event to your webhook" />
        </h4>
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
    </div>
  );
};

export default HeadersEditor;
