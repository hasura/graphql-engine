import React, { useRef } from 'react';
import { RequestTransformBodyActions } from '../../../metadata/types';
import { useDebouncedEffect } from '../../../hooks/useDebounceEffect';
import { FaExclamationCircle } from 'react-icons/fa';
import { Button } from '../../../new-components/Button';
import CrossIcon from '../Icons/Cross';
import TemplateEditor from './CustomEditors/TemplateEditor';
import JsonEditor from './CustomEditors/JsonEditor';
import AceEditor from '../AceEditor/BaseEditor';
import ResetIcon from '../Icons/Reset';
import {
  capitaliseFirstLetter,
  editorDebounceTime,
  inputStyles,
} from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import {
  KeyValuePair,
  RequestTransformStateBody,
  TransformationType,
} from './stateDefaults';
import KeyValueInput from './CustomEditors/KeyValueInput';
import { isEmpty } from '../utils/jsUtils';
import { requestBodyActionState } from './requestTransformState';

type PayloadOptionsTransformsProps = {
  transformationType: TransformationType;
  requestBody: RequestTransformStateBody;
  requestBodyError: string;
  requestSampleInput: string;
  requestTransformedBody: string;
  resetSampleInput: () => void;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
};

const PayloadOptionsTransforms: React.FC<PayloadOptionsTransformsProps> = ({
  transformationType,
  requestBody,
  requestBodyError,
  requestSampleInput,
  requestTransformedBody,
  resetSampleInput,
  requestBodyOnChange,
  requestSampleInputOnChange,
}) => {
  const editorRef = useRef<any>();
  const requestBodyTypeOptions = [
    {
      value: requestBodyActionState.remove,
      text: 'disabled',
    },
    {
      value: requestBodyActionState.transformApplicationJson,
      text: 'application/json',
    },
    {
      value: requestBodyActionState.transformFormUrlEncoded,
      text: 'application/x-www-form-urlencoded',
    },
  ];
  const [localFormElements, setLocalFormElements] = React.useState<
    KeyValuePair[]
  >(requestBody.form_template ?? [{ name: '', value: '' }]);

  React.useEffect(() => {
    setLocalFormElements(
      requestBody.form_template ?? [{ name: '', value: '' }]
    );
  }, [requestBody]);

  useDebouncedEffect(
    () => {
      requestBodyOnChange({ ...requestBody, form_template: localFormElements });
    },
    editorDebounceTime,
    [localFormElements]
  );

  if (editorRef?.current?.editor?.renderer?.$cursorLayer?.element?.style) {
    editorRef.current.editor.renderer.$cursorLayer.element.style.display =
      'none';
  }

  return (
    <div
      className="m-md pl-lg pr-sm border-l border-l-gray-400"
      data-cy="Change Payload"
    >
      <div className="mb-md">
        <NumberedSidebar
          title="Sample Input"
          description={`Sample input defined by your ${capitaliseFirstLetter(
            transformationType
          )} Defintion.`}
          number="1"
        >
          <Button
            type="button"
            size="sm"
            icon={<ResetIcon />}
            className="ml-sm"
            onClick={() => {
              resetSampleInput();
            }}
          >
            Reset
          </Button>
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
              <code className="text-xs">&#123;&#123;$body&#125;&#125;</code> to
              access the original request body
            </span>
          }
          number="2"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#request-body"
        >
          <select
            className={`ml-auto ${inputStyles}`}
            value={requestBody.action}
            onChange={e =>
              requestBodyOnChange({
                ...requestBody,
                action: e.target.value as RequestTransformBodyActions,
              })
            }
          >
            <option disabled>Request Body Type</option>
            {requestBodyTypeOptions.map(option => (
              <option key={option.value} value={option.value}>
                {option.text}
              </option>
            ))}
          </select>
        </NumberedSidebar>
        {requestBody.action ===
        requestBodyActionState.transformApplicationJson ? (
          <TemplateEditor
            requestBody={requestBody}
            requestBodyError={requestBodyError}
            requestSampleInput={requestSampleInput}
            requestBodyOnChange={requestBodyOnChange}
          />
        ) : null}

        {requestBody.action ===
        requestBodyActionState.transformFormUrlEncoded ? (
          <>
            {!isEmpty(requestBodyError) && (
              <div className="mb-sm" data-test="transform-requestBody-error">
                <CrossIcon />
                <span className="text-red-500 ml-sm">{requestBodyError}</span>
              </div>
            )}
            <div className="grid gap-3 grid-cols-3 mb-sm">
              <KeyValueInput
                pairs={localFormElements}
                setPairs={setLocalFormElements}
                testId="add-url-encoded-body"
              />
            </div>
          </>
        ) : null}

        {requestBody.action === requestBodyActionState.remove ? (
          <div className="flex items-center text-gray-600 bg-gray-200 border border-gray-400 text-sm rounded p-sm">
            <FaExclamationCircle className="mr-sm" />
            The request body is disabled. No request body will be sent with this
            {transformationType}. Enable the request body to modify your request
            transformation.
          </div>
        ) : null}
      </div>

      {requestBody.action !== requestBodyActionState.remove ? (
        <div className="mb-md">
          <NumberedSidebar
            title="Transformed Request Body"
            description="Sample request body to be delivered based on your input and
          transformation template."
            number="3"
          />
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
              useWorker: false,
            }}
            readOnly
          />
        </div>
      ) : null}
    </div>
  );
};

export default PayloadOptionsTransforms;
