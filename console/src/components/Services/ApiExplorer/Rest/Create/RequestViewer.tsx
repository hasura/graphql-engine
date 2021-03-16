import React from 'react';

import Editor from '../../../../Common/AceEditor/BaseEditor';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

import styles from '../RESTStyles.scss';

type RequestViewerProps = {
  request: string;
};

const RequestViewer: React.FC<RequestViewerProps> = ({ request }) => (
  <div className={styles.request_viewer_layout}>
    <div className={styles.request_viewer_heading}>
      <label className={styles.form_input_label}>GraphQL Request</label>
      <ToolTip message="The request your endpoint will run. All variables will be mapped to REST endpoint variables." />
    </div>
    <Editor readOnly mode="graphqlschema" value={request} height="200px" />
  </div>
);

export default RequestViewer;
