import React, { useRef } from 'react';
import TemplateEditor from './CustomEditors/TemplateEditor';
import JsonEditor from './CustomEditors/JsonEditor';
import AceEditor from '../AceEditor/BaseEditor';
import { RequestTransformContentType } from '../../../metadata/types';
import ResetIcon from '../Icons/Reset';
import { buttonShadow, focusYellowRing, inputStyles } from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';

type PayloadOptionsTransformsProps = {
  requestBody: string;
  requestBodyError: string;
  requestSampleInput: string;
  requestTransformedBody: string;
  requestContentType: RequestTransformContentType;
  resetSampleInput: () => void;
  requestBodyOnChange: (requestBody: string) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
  requestContentTypeOnChange: (
    requestContentType: RequestTransformContentType
  ) => void;
};

const PayloadOptionsTransforms: React.FC<PayloadOptionsTransformsProps> = ({
  requestBody,
  requestBodyError,
  requestSampleInput,
  requestTransformedBody,
  requestContentType,
  resetSampleInput,
  requestBodyOnChange,
  requestSampleInputOnChange,
  requestContentTypeOnChange,
}) => {
  const editorRef = useRef<any>();
  const showRequestContentTypeOptions = false;
  const requestContentTypeOptions = [
    'application/json',
    'application/x-www-form-urlencoded',
  ];

  if (editorRef?.current?.editor?.renderer?.$cursorLayer?.element?.style) {
    editorRef.current.editor.renderer.$cursorLayer.element.style.display =
      'none';
  }

  return (
    <div className="m-md pl-lg pr-sm border-l border-l-gray-400">
      <div className="mb-md">
        <NumberedSidebar
          title="Sample Input"
          description="Sample input defined by your Action Defintion."
          number="1"
        >
          <button
            type="button"
            className={`ml-auto inline-flex items-center text-sm font-medium h-btnsm px-sm mr-sm ${buttonShadow} ${focusYellowRing}`}
            onClick={() => {
              resetSampleInput();
            }}
          >
            <ResetIcon />
            Reset
          </button>
        </NumberedSidebar>
        <JsonEditor
          value={requestSampleInput}
          onChange={requestSampleInputOnChange}
        />
      </div>

      <div className="mb-md">
        <NumberedSidebar
          title="Configure Request Body"
          description={
            <span>
              The template which will transform your request body into the
              required specification. You can use{' '}
              <code className="text-xs">$body</code> to access the original
              request body
            </span>
          }
          number="2"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#request-body"
        />
        <TemplateEditor
          requestBody={requestBody}
          requestBodyError={requestBodyError}
          requestSampleInput={requestSampleInput}
          requestBodyOnChange={requestBodyOnChange}
        />
      </div>

      <div className="mb-md">
        <NumberedSidebar
          title="Transformed Request Body"
          description="Sample request body to be delivered based on your input and
          transformation template."
          number="3"
        >
          {showRequestContentTypeOptions ? (
            <select
              className={`ml-auto ${inputStyles}`}
              value={requestContentType}
              onChange={e =>
                requestContentTypeOnChange(
                  e.target.value as RequestTransformContentType
                )
              }
            >
              <option disabled>Data Type</option>
              {requestContentTypeOptions.map(option => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          ) : null}
        </NumberedSidebar>
        <AceEditor
          mode="json"
          editorRef={editorRef}
          value={requestTransformedBody}
          showPrintMargin={false}
          highlightActiveLine={false}
          height="200px"
          width="100%"
          fontSize="12px"
          style={{ background: '#e2e8f0' }}
          setOptions={{
            highlightGutterLine: false,
          }}
          readOnly
        />
      </div>
    </div>
  );
};

export default PayloadOptionsTransforms;
