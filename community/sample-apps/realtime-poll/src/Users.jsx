import { useSubscription } from "@apollo/client";
import React from "react";
import { Alert } from "react-bootstrap";
import { Error, Loading } from "./Components";
import { SUBSCRIPTION_ONLINE_USERS } from "./GraphQL";

export const Users = () => {
  const { data, loading, error } = useSubscription(SUBSCRIPTION_ONLINE_USERS);
  const { count } = data?.online_users[0] || {};

  if (loading) return <Loading />;
  if (error) return <Error message={error.message} />;

  return (
    <div className="displayFlex online-users">
      <div className="col-md-6">
        <Alert variant="info">
          <span role="img" aria-label="online users">
            👥
          </span>{" "}
          Online users: {count}
        </Alert>
      </div>
    </div>
  );
};
