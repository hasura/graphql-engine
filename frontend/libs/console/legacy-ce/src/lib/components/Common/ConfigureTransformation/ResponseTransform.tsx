import React, { useRef } from 'react';
import { useDebouncedEffect } from '../../../hooks/useDebounceEffect';
import TemplateEditor from './CustomEditors/TemplateEditor';
import { editorDebounceTime } from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { KeyValuePair, ResponseTransformStateBody } from './stateDefaults';
import { responseBodyActionState } from './requestTransformState';

type PayloadOptionsTransformsProps = {
  responseBody: ResponseTransformStateBody;
  responseBodyOnChange: (responseBody: ResponseTransformStateBody) => void;
};

const ResponseTransforms: React.FC<PayloadOptionsTransformsProps> = ({
  responseBody,
  responseBodyOnChange,
}) => {
  const editorRef = useRef<any>();

  const [localFormElements, setLocalFormElements] = React.useState<
    KeyValuePair[]
  >(responseBody.form_template ?? [{ name: '', value: '' }]);

  React.useEffect(() => {
    setLocalFormElements(
      responseBody.form_template ?? [{ name: '', value: '' }]
    );
  }, [responseBody]);

  useDebouncedEffect(
    () => {
      responseBodyOnChange({
        ...responseBody,
        form_template: localFormElements,
      });
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
      data-cy="Change Response"
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
        {responseBody.action ===
        responseBodyActionState.transformApplicationJson ? (
          <TemplateEditor
            requestBody={responseBody}
            requestBodyOnChange={responseBodyOnChange}
          />
        ) : null}
      </div>
    </div>
  );
};

export default ResponseTransforms;
