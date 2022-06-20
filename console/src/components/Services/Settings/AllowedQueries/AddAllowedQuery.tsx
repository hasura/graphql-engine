import React, { useState } from 'react';
import AceEditor from 'react-ace';
import { isConsoleError } from '@/components/Common/utils/jsUtils';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from '../../../Common/Tooltip/Tooltip';

import { readFile, parseQueryString, renameDuplicates } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { addAllowedQueries } from '../../../../metadata/actions';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';
import { Dispatch } from '../../../../types';
import { inputStyles } from '../constants';

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
        if (isConsoleError(error)) {
          dispatch(
            showErrorNotification('Uploading operations failed', error.message)
          );
        }
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
        <div className="mb-sm">
          <b>Query name:</b>
          <Tooltip
            message="This is an identifier for the query in the collection. 
          This should be unique in the collection and can be different from the operation name of the query."
          />
        </div>
        <input
          type="text"
          className={`${inputStyles} inline-block`}
          placeholder="operation_name"
          value={manualQuery.name}
          onChange={handleNameChange}
        />
      </div>
      <div className="mt-md">
        <div>
          <div className="mb-sm">
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
      <div className="mb-sm">
        <b>Graphql File:</b>
        <Tooltip message=".graphql file with operations" />
      </div>
      <input
        type="file"
        className={`${inputStyles} inline-block`}
        onChange={handleFileUpload}
      />
    </div>
  );

  return (
    <div>
      <h4 className="text-lg font-bold pb-sm">
        Add new operations to allow-list
      </h4>
      <div className="px-sm">
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
        <div className="mt-md">OR</div>
        <div className="mt-md">
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
