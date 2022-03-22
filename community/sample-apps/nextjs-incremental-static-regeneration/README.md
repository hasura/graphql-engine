# Trigger Next.js Incremental Static Regeneration with a Hasura Table Event

New in Next.js 12.1 is [Incremental Static Regeneration](https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration) which allows us to create and update pages on demand. We can pair this with Hasura table events to keep our web pages always up to date and only rebuild when data changes.

Lets setup an example blog app to check this out.

## Setup Hasura

1. [Download the Hasura Docker Compose file.](https://hasura.io/docs/latest/graphql/core/getting-started/docker-simple.html#step-1-get-the-docker-compose-file)

1. In the Docker Compose graphql-engine environment variable section add `SECRET_TOKEN: <a random string you come up with>`

1. [Start Hasura and launch the console.](https://hasura.io/docs/latest/graphql/core/getting-started/docker-simple.html#step-2-run-hasura-graphql-engine)

1. In the data tab we create a new table `post` with the following columns:

   - `id` UUID from the frequently used columns
   - `title` type text
   - `content` type text

1. In the API tab we construct our GraphQL query

```graphql
{
  post {
    content
    id
    title
  }
}
```

## Setup Next.js

1. We create an example typescript Next.js app `npx create-next-app@latest --ts`

1. We will query Hasura using [graphql-request](https://github.com/prisma-labs/graphql-request). Install with `npm install graphql-request graphql`

1. In `index.tsx` we setup our getStaticProps data fetching

   ```typescript
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
   ```

1. Finally we display our blog posts

   ```typescript
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
   ```

## Setup Incremental Static Regeneration

When running Next.js in production mode if we add a new blog post in Hasura Next.js has no way to know about it. In the past, before Incremental Static Regeneration, we would have to set a [revalidate time](https://nextjs.org/docs/api-reference/data-fetching/get-static-props#revalidate), where it would rebuild a page every x amount of time even if there wasn't anything new.

Now we can setup a webhook where we can tell Next.js to rebuild specific pages when our data changes! [Following the Next.js on-demand revalidation guide:](https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration#using-on-demand-revalidation)

1. When we setup our Docker Compose we came up with a `SECRET_TOKEN`, we use that as a password to communicate with Next.js. Create .env.local file

```env
SECRET_TOKEN=<Same as what you set in Docker Compose>
```

1. Create the revalidation API route, the only difference from the official example is we look for the secret token in the header instead of a query variable.

```typescript
// pages/api/revalidate.ts
// From Next.js docs https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration#using-on-demand-revalidation
import type { NextApiRequest, NextApiResponse } from "next";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  // Check for secret to confirm this is a valid request
  if (req.headers.secret !== process.env.SECRET_TOKEN) {
    return res.status(401).json({ message: "Invalid token" });
  }

  try {
    await res.unstable_revalidate("/");
    return res.json({ revalidated: true });
  } catch (err) {
    // If there was an error, Next.js will continue
    // to show the last successfully generated page
    return res.status(500).send("Error revalidating");
  }
}
```

1. Back in the Hasura console events tab, create an event trigger.

- Name the trigger anything, select the post table, and select all trigger operations.
- With docker, the webhook handler should be `http://host.docker.internal/api/revalidate`
- Under Advanced Settings we add the SECRET header from the `SECRET_TOKEN` environment variable

1. Save the event trigger and [run Next.js in production mode](https://nextjs.org/docs/basic-features/data-fetching/incremental-static-regeneration#testing-on-demand-isr-during-development)

```bash
npm run build
npm run start
```

Now when you add a post in Hasura, when you refresh you Next.js app you'll see your updated data!

## Contributing

Checkout the [contributing guide](../../../CONTRIBUTING.md#community-content) for more details.
