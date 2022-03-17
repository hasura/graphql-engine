import type { NextPage } from "next";
import { request, gql } from "graphql-request";

interface Props {
  posts: {
    id: string;
    title: string;
    content: string;
  }[];
}

const query = gql`
  {
    post {
      content
      id
      title
    }
  }
`;

export async function getStaticProps() {
  const { post: posts } = await request(
    "http://localhost:8080/v1/graphql",
    query
  );
  return {
    props: {
      posts,
    },
  };
}

const Home: NextPage<Props> = ({ posts }) => {
  return (
    <main>
      {posts.map((post) => (
        <article key={post.id}>
          <h2>{post.title}</h2>
          <p>{post.content}</p>
        </article>
      ))}
    </main>
  );
};

export default Home;
