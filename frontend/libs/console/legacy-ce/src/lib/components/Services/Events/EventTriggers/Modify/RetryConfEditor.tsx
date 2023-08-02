import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { EventTrigger, RetryConf, VoidCallback } from '../../types';
import CommonRetryConf from '../../Common/Components/RetryConfEditor';

type RetryConfEditorProps = {
  currentTrigger: EventTrigger;
  conf: RetryConf;
  setRetryConf: (r: RetryConf) => void;
  save: (success: VoidCallback, error: VoidCallback) => void;
};

const tdHeadStyle = 'px-3 py-3 whitespace-nowrap font-medium';
const tdValStyle = 'px-3 py-3 whitespace-nowrap text-gray-600';

const RetryConfEditor = (props: RetryConfEditorProps) => {
  const { currentTrigger, conf, setRetryConf, save } = props;

  const existingConf = currentTrigger.configuration.retry_conf;

  const reset = () => {
    setRetryConf(existingConf);
  };

  const collapsed = () => (
    <div className="overflow-x-auto border border-gray-300 rounded mb-sm">
      <table className="min-w-full divide-y divide-gray-200">
        <tbody className="bg-white divide-y divide-gray-200">
          <tr className="">
            <td className={tdHeadStyle}>Number of Retries</td>
            <td className={tdValStyle}>{existingConf.num_retries || 0}</td>
          </tr>
          <tr className="">
            <td className={tdHeadStyle}>Retry Interval (sec)</td>
            <td className={tdValStyle}>{existingConf.interval_sec || 10}</td>
          </tr>
          <tr className="">
            <td className={tdHeadStyle}>Timeout (sec)</td>
            <td className={tdValStyle}>{existingConf.timeout_sec || 60}</td>
          </tr>
        </tbody>
      </table>
    </div>
  );

  const expanded = () => (
    <CommonRetryConf
      retryConf={conf}
      setRetryConf={setRetryConf}
      legacyTooltip={false}
    />
  );

  return (
    <div className="my-lg mt-sm w-6/12">
      <h2 className="text-lg font-bold mb-xs items-center ">
        Retry Configuration
      </h2>
      <p className="text-sm mb-sm text-gray-600">
        Edit your retry setting for event failures.
      </p>
      <Editor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        property="retry"
        saveFunc={save}
        service="modify-trigger"
        expandCallback={reset}
        dataTest="edit-retry-config"
      />
    </div>
  );
};

export default RetryConfEditor;
