import React from 'react';
import AceEditor from 'react-ace';
import 'brace/mode/json';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import { subHeading } from '../../constants';

type Props = {
  requestPayload: string;
  responsePayload: string;
};

const InvocationLogDetails = ({ requestPayload, responsePayload }: Props) => {
  return (
    <div className="p-md">
      <Tabs animation={false} defaultActiveKey={1} id="requestResponseTab">
        <Tab eventKey={1} title="Request">
          <div className="mt-sm">
            <div className={subHeading}>Request</div>
            <AceEditor
              mode="json"
              theme="github"
              name="payload"
              value={requestPayload}
              minLines={4}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              setOptions={{ useWorker: false }}
            />
          </div>
        </Tab>
        <Tab eventKey={2} title="Response">
          <div className="mt-sm">
            <div className={subHeading}>Response</div>
            <AceEditor
              mode="json"
              theme="github"
              name="response"
              value={responsePayload}
              minLines={4}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              showGutter={false}
              setOptions={{ useWorker: false }}
            />
          </div>
        </Tab>
      </Tabs>
    </div>
  );
};

export default InvocationLogDetails;
