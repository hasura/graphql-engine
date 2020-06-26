import React from "react";
import graphql from "babel-plugin-relay/macro";
import {
  RelayEnvironmentProvider,
  preloadQuery,
  usePreloadedQuery,
} from "react-relay/hooks";
import RelayEnvironment from "./RelayEnvironment";
import { Badge } from "./components/shared/Badge";
import RestaurantReviews from "./components/RestaurantReviews";

const { Suspense } = React;

// Define a query
const RestaurantsQuery = graphql`
  query AppRestaurantsQuery($id: uuid!) {
    restaurants_connection(where: { id: { _eq: $id } }) {
      edges {
        node {
          name
          cuisine
          ...RestaurantReviews_restaurants
        }
        cursor
      }
    }
  }
`;

// Immediately load the query as our app starts. For a real app, we'd move this
// into our routing configuration, preloading data as we transition to new routes.
const preloadedQuery = preloadQuery(RelayEnvironment, RestaurantsQuery, {
  id: "MY_RESTAURANT_ID",
});

// Inner component that reads the preloaded query results via `usePreloadedQuery()`.
function App(props) {
  const data = usePreloadedQuery(RestaurantsQuery, props.preloadedQuery);
  const restaurant = data.restaurants_connection.edges[0].node;
  const { name, cuisine } = data.restaurants_connection.edges[0].node;

  return (
    <div>
      <h3>
        {name} <Badge>{cuisine}</Badge>
      </h3>
      <RestaurantReviews restaurant={restaurant} />
    </div>
  );
}

// The above component needs to know how to access the Relay environment, and we
// need to specify a fallback in case it suspends:
// - <RelayEnvironmentProvider> tells child components how to talk to the current
//   Relay Environment instance
// - <Suspense> specifies a fallback in case a child suspends.
function AppRoot(props) {
  return (
    <RelayEnvironmentProvider environment={RelayEnvironment}>
      <Suspense fallback={"Loading..."}>
        <App preloadedQuery={preloadedQuery} />
      </Suspense>
    </RelayEnvironmentProvider>
  );
}

export default AppRoot;
