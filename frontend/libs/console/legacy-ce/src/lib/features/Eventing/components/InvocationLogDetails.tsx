import { JsonCodeBlock, Tabs } from '@hasura/shared/ui';

type Props = {
  requestPayload: unknown;
  responsePayload: unknown;
};

const InvocationLogDetails = ({ requestPayload, responsePayload }: Props) => {
  return (
    <Tabs
      defaultValue="request"
      items={[
        {
          value: 'request',
          label: 'Request',
          content: <JsonCodeBlock value={requestPayload} hideCopyButton />,
        },
        {
          value: 'response',
          label: 'Response',
          content: <JsonCodeBlock value={responsePayload} hideCopyButton />,
        },
      ]}
    />
  );
};

export default InvocationLogDetails;
