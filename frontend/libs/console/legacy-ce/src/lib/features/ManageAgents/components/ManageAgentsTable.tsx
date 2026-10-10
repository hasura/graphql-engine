import { Button, CardedTable, IndicatorCard, Text } from '@hasura/shared/ui';
import { FaTrash } from 'react-icons/fa';
import { useListAvailableAgentsFromMetadata } from '../hooks';
import { useRemoveAgent } from '../hooks/useRemoveAgent';
import { DataConnectorUri } from '@hasura/shared/types';
import { Em, Skeleton } from '@radix-ui/themes';
import type { JSX } from 'react';

export const ManageAgentsTable = () => {
  const { data, isFetching, error } = useListAvailableAgentsFromMetadata();
  const { removeAgent, isPending } = useRemoveAgent();

  if (error) {
    return (
      <IndicatorCard className="my-4" showIcon status="negative">
        Something went wrong while fetching data
      </IndicatorCard>
    );
  }

  if (!isFetching && !data?.length)
    return (
      <IndicatorCard className="my-4">
        <Text>
          <Em>There are no data connector agents connected to Hasura.</Em>
        </Text>
      </IndicatorCard>
    );

  return (
    <div className="my-4">
      <Skeleton loading={isFetching}>
        <CardedTable
          columns={['Agent Name', 'URL', 'Actions']}
          data={
            data?.map((agent) => {
              return [
                agent.name,
                <DataConnectorUriLabel
                  key={`${agent.name}-uri`}
                  uri={agent.uri}
                />,
                <Button
                  key={`${agent.name}-remove`}
                  mode="destructive"
                  leftIcon={FaTrash}
                  disabled={isPending}
                  onClick={() => {
                    removeAgent({ name: agent.name });
                  }}
                >
                  Remove
                </Button>,
              ];
            }) ?? []
          }
        />
      </Skeleton>
    </div>
  );
};

const DataConnectorUriLabel = (props: {
  uri: DataConnectorUri;
}): JSX.Element => {
  if (typeof props.uri === 'string') {
    return <>{props.uri}</>;
  } else {
    return (
      <>
        Environment Variable: <code>{props.uri.from_env}</code>
      </>
    );
  }
};
