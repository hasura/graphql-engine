import React, { useRef } from 'react';
import { useDebouncedEffect } from '@/hooks/useDebounceEffect';
import CrossIcon from '../Icons/Cross';
import TemplateEditor from './CustomEditors/TemplateEditor';
import { editorDebounceTime } from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { KeyValuePair, RequestTransformStateBody } from './stateDefaults';
import KeyValueInput from './CustomEditors/KeyValueInput';
import { isEmpty } from '../utils/jsUtils';
import { requestBodyActionState } from './requestTransformState';

type PayloadOptionsTransformsProps = {
  requestBody: RequestTransformStateBody;
  requestBodyError: string;
  requestSampleInput: string;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
};

const ResponseTransforms: React.FC<PayloadOptionsTransformsProps> = ({
  requestSampleInput,
  requestBody,
  requestBodyError,
  requestBodyOnChange,
}) => {
  const editorRef = useRef<any>();

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
          title="Configure Response Body"
          description={
            <span>
              The template which will transform your response body into the
              required specification. You can use{' '}
              <code className="text-xs">&#123;&#123;$body&#125;&#125;</code> to
              access the original response body
            </span>
          }
          number="1"
        />
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
      </div>
    </div>
  );
};

export default ResponseTransforms;
