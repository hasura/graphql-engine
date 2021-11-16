import React, { useState } from 'react';
import AceEditor from 'react-ace';
import styles from './AllowedQueries.scss';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import { readFile, parseQueryString, renameDuplicates } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { addAllowedQueries } from '../../../../metadata/actions';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';
import { Dispatch } from '../../../../types';

const defaultManualQuery: AllowedQueriesCollection = {
  name: '',
  query: '',
  collection: allowedQueriesCollection,
};

type AddAllowedQueryProps = {
  dispatch: Dispatch;
  allowedQueries: AllowedQueriesCollection[];
};

const AddAllowedQuery: React.FC<AddAllowedQueryProps> = props => {
  const { dispatch, allowedQueries } = props;

  const [manualQuery, setManualQuery] = useState<AllowedQueriesCollection>(
    defaultManualQuery
  );
  const [graphqlFile, setGraphqlFile] = useState<File | null>(null);

  const handleManualCollapse = () => {
    setManualQuery(defaultManualQuery);
  };

  const handleManualSubmit = (toggle: () => void) => {
    dispatch(addAllowedQueries([manualQuery], toggle));
  };

  const handleFileUploadCollapse = () => {};

  const handleFileUploadSubmit = (toggle: () => void) => {
    const addFileQueries = (content: string) => {
      try {
        const fileQueries = parseQueryString(content);
        const updatedQueries = renameDuplicates(fileQueries, allowedQueries);
        dispatch(addAllowedQueries(updatedQueries, toggle));
      } catch (error) {
        dispatch(
          showErrorNotification('Uploading operations failed', error.message)
        );
      }
    };

    readFile(graphqlFile, addFileQueries);
  };

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setManualQuery({
      ...manualQuery,
      name: e.target.value,
    });
  };

  const handleQueryChange = (val: string) => {
    setManualQuery({
      ...manualQuery,
      query: val,
    });
  };

  const manualQueryInput = () => (
    <div>
      <div>
        <div className={styles.add_mar_bottom_mid}>
          <b>Query name:</b>
          <Tooltip
            message="This is an identifier for the query in the collection. 
          This should be unique in the collection and can be different from the operation name of the query."
          />
        </div>
        <input
          type="text"
          className={`form-control input-sm ${styles.inline_block}`}
          placeholder="operation_name"
          value={manualQuery.name}
          onChange={handleNameChange}
        />
      </div>
      <div className={styles.add_mar_top}>
        <div>
          <div className={styles.add_mar_bottom_mid}>
            <b>Operation:</b>
          </div>
          <AceEditor
            data-test="allowed_operation_add"
            mode="graphql"
            theme="github"
            name="allowed_operation_add"
            value={manualQuery.query}
            minLines={8}
            maxLines={100}
            width="100%"
            showPrintMargin={false}
            onChange={handleQueryChange}
          />
        </div>
      </div>
    </div>
  );

  const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;
    setGraphqlFile(files![0]);
  };

  const fileUploadInput = () => (
    <div>
      <div className={styles.add_mar_bottom_mid}>
        <b>Graphql File:</b>
        <Tooltip message=".graphql file with operations" />
      </div>
      <input
        type="file"
        className={`form-control input-sm ${styles.inline_block}`}
        onChange={handleFileUpload}
      />
    </div>
  );

  return (
    <div>
      <h4 className={styles.subheading_text}>
        Add new operations to allow-list
      </h4>
      <div className={styles.subsection}>
        <div>
          <ExpandableEditor
            expandButtonText="Add operation manually"
            editorExpanded={manualQueryInput}
            collapseCallback={handleManualCollapse}
            property="add-allowed-operation"
            service="add-allowed-operation"
            saveButtonText="Add"
            saveFunc={handleManualSubmit}
          />
        </div>
        <div className={styles.add_mar_top}>OR</div>
        <div className={styles.add_mar_top}>
          <ExpandableEditor
            expandButtonText="Upload graphql file"
            editorExpanded={fileUploadInput}
            collapseCallback={handleFileUploadCollapse}
            property="upload-allowed-operations"
            service="upload-allowed-operations"
            saveButtonText="Upload"
            saveFunc={handleFileUploadSubmit}
          />
        </div>
      </div>
    </div>
  );
};

export default AddAllowedQuery;
