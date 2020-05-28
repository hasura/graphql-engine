import React from 'react';
import AceEditor from 'react-ace';
import 'brace/mode/json';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import styles from '../../Events.scss';

type Props = {
  requestPayload: string;
  responsePayload: string;
};

const InvocationLogDetails = ({ requestPayload, responsePayload }: Props) => {
  return (
    <div className={styles.addPadding20Px}>
      <Tabs animation={false} defaultActiveKey={1} id="requestResponseTab">
        <Tab eventKey={1} title="Request">
          <div className={styles.add_mar_top}>
            <div className={styles.subheading_text}>Request</div>
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
            />
          </div>
        </Tab>
        <Tab eventKey={2} title="Response">
          <div className={styles.add_mar_top}>
            <div className={styles.subheading_text}>Response</div>
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
            />
          </div>
        </Tab>
      </Tabs>
    </div>
  );
};

export default InvocationLogDetails;
