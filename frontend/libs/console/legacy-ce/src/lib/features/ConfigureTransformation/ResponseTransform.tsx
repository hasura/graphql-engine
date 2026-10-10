import React from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import TemplateEditor from './CustomEditors/TemplateEditor';
import { editorDebounceTime } from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { ResponseTransformStateBody } from './stateDefaults';
import { responseBodyActionState } from './requestTransformState';
import { NameValue } from '@hasura/shared/types';
import { Code } from '@radix-ui/themes';

type PayloadOptionsTransformsProps = {
  responseBody: ResponseTransformStateBody;
  responseBodyOnChange: (responseBody: ResponseTransformStateBody) => void;
};

const ResponseTransforms: React.FC<PayloadOptionsTransformsProps> = ({
  responseBody,
  responseBodyOnChange,
}) => {
  const [localFormElements, setLocalFormElements] = React.useState<NameValue[]>(
    responseBody.form_template ?? [{ name: '', value: '' }],
  );

  React.useEffect(() => {
    setLocalFormElements(
      responseBody.form_template ?? [{ name: '', value: '' }],
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
    [localFormElements],
  );

  return (
    <div
      className="m-4 pl-10 pr-2 border-l border-l-(--gray-a7)"
      data-cy="Change Response"
    >
      <div className="mb-4">
        <NumberedSidebar
          title="Configure Response Body"
          description={
            <span>
              The template which will transform your response body into the
              required specification. You can use{' '}
              <Code>&#123;&#123;$body&#125;&#125;</Code> to access the original
              response body
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
