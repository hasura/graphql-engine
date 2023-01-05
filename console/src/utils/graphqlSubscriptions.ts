import { SubscriptionClient } from 'subscriptions-transport-ws';

export const getGraphqlSubscriptionsClient = (
  url: string,
  headers: Record<string, string>
) => {
  return new SubscriptionClient(url, {
    connectionParams: {
      headers: {
        ...headers,
      },
      lazy: true,
    },
    reconnect: true,
  });
};
