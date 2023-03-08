import { useQuery, gql } from "@apollo/client";
import AuthorList from "./AuthorList";
import withApollo from "../config";

const QUERY = gql`
  query {
    author {
      id
      name
    }
  }
`;

const Index = () => {
  const { data, loading, error } = useQuery(QUERY);

  if (loading) {
    return <h2>Loading...</h2>;
  }

  if (error) {
    return <h2>Error..</h2>;
  }

  return (
    <div>
      <h1>My Authors </h1>
      <AuthorList authors={data ? data.author : []} />
    </div>
  );
};

export default withApollo(Index);
