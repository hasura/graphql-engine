import React from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { FaExclamationCircle } from 'react-icons/fa';
import {
  Button,
  IndicatorCard,
  JsonCodeBlock,
  Select,
  Text,
} from '@hasura/shared/ui';
import CrossIcon from '../../components/Common/Icons/Cross';
import TemplateEditor from './CustomEditors/TemplateEditor';
import JsonEditor from './CustomEditors/JsonEditor';
import ResetIcon from '../../components/Common/Icons/Reset';
import { editorDebounceTime } from './utils';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { RequestTransformStateBody, TransformationType } from './stateDefaults';
import KeyValueInput from './CustomEditors/KeyValueInput';
import { capitalizeFirstLetter, isEmpty } from '@hasura/shared/utils';
import { requestBodyActionState } from './requestTransformState';
import { Code, Flex } from '@radix-ui/themes';
import { NameValue } from '@hasura/shared/types';
import { RequestTransformBodyActions } from '@hasura/shared/types';

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

const requestBodyTypeOptions = [
  {
    value: requestBodyActionState.remove,
    label: 'disabled',
  },
  {
    value: requestBodyActionState.transformApplicationJson,
    label: 'application/json',
  },
  {
    value: requestBodyActionState.transformFormUrlEncoded,
    label: 'application/x-www-form-urlencoded',
  },
];

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
  const [localFormElements, setLocalFormElements] = React.useState<NameValue[]>(
    requestBody.form_template ?? [{ name: '', value: '' }],
  );

  React.useEffect(() => {
    setLocalFormElements(
      requestBody.form_template ?? [{ name: '', value: '' }],
    );
  }, [requestBody]);

  useDebouncedEffect(
    () => {
      requestBodyOnChange({ ...requestBody, form_template: localFormElements });
    },
    editorDebounceTime,
    [localFormElements],
  );

  return (
    <div
      className="my-4 ml-6 pl-10 border-l border-l-(--gray-a7)"
      data-cy="Change Payload"
    >
      <div className="mb-4">
        <NumberedSidebar
          title="Sample Input"
          description={`Sample input defined by your ${capitalizeFirstLetter(
            transformationType,
          )} Defintion.`}
          number="1"
        >
          <Button
            type="button"
            size="sm"
            mode="default"
            leftIcon={ResetIcon}
            className="ml-2"
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

      <div className="mb-4">
        <NumberedSidebar
          title="Configure Request Body"
          description={
            <span>
              The template which will transform your request body into the
              required specification. You can use{' '}
              <Code>&#123;&#123;$body&#125;&#125;</Code> to access the original
              request body
            </span>
          }
          number="2"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#request-body"
        >
          <Select
            value={requestBody.action}
            placeholder="Request Body Type"
            options={requestBodyTypeOptions}
            onChange={(value) =>
              requestBodyOnChange({
                ...requestBody,
                action: value as RequestTransformBodyActions,
              })
            }
          />
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
              <Flex
                className="mb-2"
                align="center"
                gap="2"
                data-test="transform-requestBody-error"
              >
                <CrossIcon />
                <Text color="red">{requestBodyError}</Text>
              </Flex>
            )}
            <KeyValueInput
              pairs={localFormElements}
              setPairs={setLocalFormElements}
              testId="add-url-encoded-body"
            />
          </>
        ) : null}

        {requestBody.action === requestBodyActionState.remove ? (
          <IndicatorCard showIcon customIcon={FaExclamationCircle}>
            The request body is disabled. No request body will be sent with this
            {transformationType}. Enable the request body to modify your request
            transformation.
          </IndicatorCard>
        ) : null}
      </div>

      {requestBody.action !== requestBodyActionState.remove ? (
        <div className="mb-4">
          <NumberedSidebar
            title="Transformed Request Body"
            description="Sample request body to be delivered based on your input and
          transformation template."
            number="3"
          />
          <JsonCodeBlock value={requestTransformedBody} />
        </div>
      ) : null}
    </div>
  );
};

export default PayloadOptionsTransforms;
