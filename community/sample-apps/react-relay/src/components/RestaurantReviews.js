import React, { Suspense } from "react";
import graphql from "babel-plugin-relay/macro";
import { usePaginationFragment } from "react-relay/hooks";
import { List, ListItem } from "./shared/List";
import { Button } from "./shared/Button";

function RestaurantReviews(props) {
  const { data, loadNext, hasNext, isLoadingNext } = usePaginationFragment(
    graphql`
      fragment RestaurantReviews_restaurants on restaurants
        @argumentDefinitions(
          cursor: { type: "String" }
          first: { type: "Int", defaultValue: 3 }
        )
        @refetchable(queryName: "RestaurantReviewsPaginationQuery") {
        reviews_connection(
          first: $first
          after: $cursor
          order_by: { created_at: desc }
        ) @connection(key: "Restaurant_reviews_connection") {
          edges {
            node {
              id
              body
              created_at
            }
          }
        }
      }
    `,
    props.restaurant
  );

  return (
    <>
      <Suspense fallback={"Loading..."}>
        <List>
          {(data.reviews_connection?.edges ?? []).map((edge) => {
            const { id, body } = edge.node;
            return <ListItem key={id}>{body}</ListItem>;
          })}
        </List>
      </Suspense>

      {/* Only render button if there are more reviews to load */}
      {hasNext ? (
        <div style={{ textAlign: "center" }}>
          <Button
            type="button"
            onClick={() => {
              loadNext(3);
            }}
            disabled={isLoadingNext}
          >
            {isLoadingNext ? "Loading..." : "Load More"}
          </Button>
        </div>
      ) : null}
    </>
  );
}

export default RestaurantReviews;
