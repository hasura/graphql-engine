# Next.js 13 with GraphQL

Next.js, the open-source React framework by Vercel, has released version 13, laying the foundations to be dynamic without limits. Please read their [release blog post](https://nextjs.org/blog/next-13) for an excellent overview.

In this post, we will build a mini e-commerce store where you can view the products on sale, order one, and see a real-time feed of orders. We are covering new Next.js features such as:

- Nested Layouts
- Data Fetching
- Streaming and Suspense
- Client and Server Components

We also see how to integrate with Hasura GraphQL for excellent features such as [Streaming Subscriptions](https://hasura.io/docs/latest/subscriptions/postgres/streaming/index/).

Let’s dive in with the e-commerce schema from [the Hasura Super App](https://hasura.io/reference-app/). See our [Data Hub](https://hasura.io/data-hub/data-models-and-authorization/schema-share-data-model-ecommerce/) for instructions on how to import the schema into Hasura.

To run this sample app:

```bash
docker compose up -d

npm run dev
```

## Next.js Setup

Create a Next.js TypeScript application. As of this blog post, we will use the experimental version because that allows us to use beta features.

```bash
npx create-next-app@latest --experimental-app
```

Install the dependencies

```bash
npm install @graphql-codegen/cli @graphql-codegen/client-preset @graphql-typed-document-node/core dotenv graphql graphql-request graphql-ws
```

Replace the contents of `next.config.js`

```javascript
/** @type {import('next').NextConfig} */
const nextConfig = {
  experimental: {
    appDir: true,
    runtime: "experimental-edge",
  },
  images: {
    remotePatterns: [
      {
        protocol: "http",
        hostname: "img6a.flixcart.com",
      },
      {
        protocol: "http",
        hostname: "img5a.flixcart.com",
      },
    ],
  },
};

module.exports = nextConfig;
```

The `experimental-edge` runtime lets us use [Vercel's edge runtime](https://edge-runtime.vercel.app/). The term “Edge” refers to the orientation toward instant serverless compute environments and not a specific set of locations. The image section configures [Next.js's image component](https://beta.nextjs.org/docs/optimizing/images) to use pictures from our e-commerce store.

Our last step is adding two entries to `.env.local`

```env
NEXT_PUBLIC_HASURA_GRAPHQL_URL=<Hasura GraphQL endpoint>
NEXT_PUBLIC_HASURA_GRAPHQL_WS_URL=<Hasura websocket endpoint>
```

If your Hasura instance has an admin secret, you can also set `HASURA_GRAPHQL_ADMIN_SECRET` for use in the code generator.

## GraphQL Code Generator Setup

When making TypeScript applications with GraphQL we highly recommend using [GraphQL Code Generator](https://www.the-guild.dev/graphql/codegen). Combined with the [graphql-request](https://github.com/prisma-labs/graphql-request) library, we have fully typed queries and mutations by the magic of code generation!

Create a file `codegen.ts`

```typescript
import type { CodegenConfig } from "@graphql-codegen/cli";

const config: CodegenConfig = {
  overwrite: true,
  schema: [
    {
      [process.env.NEXT_PUBLIC_HASURA_GRAPHQL_URL!]: {
        headers: {
          // If you have an admin secret set
          "x-hasura-admin-secret": process.env.HASURA_GRAPHQL_ADMIN_SECRET!,
        },
      },
    },
  ],
  config: {
    skipTypename: true,
    enumsAsTypes: true,
    scalars: {
      numeric: "number",
    },
  },
  documents: "lib/service/queries.graphql",
  generates: {
    "lib/gql/": {
      preset: "client",
      config: {},
      plugins: [],
    },
  },
};

export default config;
```

This config connects to our Hasura GraphQL instance and generates typings based on the client preset.

In `package.json` add a script to load environment variables and run the code generator `"codegen": "graphql-codegen --require dotenv/config --config codegen.ts dotenv_config_path=./.env.local"`

Next, define our queries/mutations in `lib/service/queries.graphql`

```graphql
query GetProducts {
  product(limit: 10) {
    id
    name
  }
}

query GetProduct($id: Int!) {
  product_by_pk(id: $id) {
    id
    description
    name
    price
    image_urls(path: "$[0]")
  }
}

mutation PlaceOrder($products: order_product_arr_rel_insert_input!) {
  insert_order_one(
    object: {
      products: $products
      billing_address_id: 222
      user_id: 225
      shipping_address_id: 222
    }
  ) {
    id
  }
}
```

Finally, run the generator:

```bash
npm run codegen
```

## graphql-request Setup

Create a `graphql-request` client in `lib/service/client.ts`

```typescript
import { GraphQLClient } from "graphql-request";

export const gqlClient = new GraphQLClient(
  process.env.NEXT_PUBLIC_HASURA_GRAPHQL_URL!,
  { fetch }
);
```

We pass in the environment’s global fetch, so our Next.js will work anywhere, regular Node.js or an edge runtime.

## Nested Layouts

One of the new Next.js features is [nested layouts](https://beta.nextjs.org/docs/routing/pages-and-layouts#layouts). They allow us to have a shared parent component, and the router can render in child routes.

To demonstrate this, we will display ten products on our home page. When you click on one, it will navigate the right-hand side to the product details page.

### Fetch Home Page Data

Next.js 13 uses React’s new way of fetching data. Read more about the [RFC on Github](https://github.com/reactjs/rfcs/pull/229). We will use it to get a list of products.

In `app/layout.tsx`, we add an async function to get data from Hasura:

```typescript
import Link from "next/link";
import { GetProductsDocument } from "../lib/gql/graphql";
import { gqlClient } from "../lib/service/client";
import "./globals.css";

async function getProducts() {
  const { product: products } = await gqlClient.request(
    GetProductsDocument,
    {}
  );
  return products;
}
```

We call it in our component using async/await, then create an unordered list of product links:

```tsx
export default async function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const products = await getProducts();
  return (
    <html lang="en">
      <head />
      <body>
        <main style={{ display: "flex" }}>
          <section style={{ minWidth: "400px", width: "400px" }}>
            <ul>
              {products.map((product) => (
                <li key={product.id}>
                  <Link href={`/${product.id}`}>{product.name}</Link>
                </li>
              ))}
            </ul>
          </section>
          <section style={{ flexGrow: 1 }}>{children}</section>
        </main>
      </body>
    </html>
  );
}
```

With nested layouts and a `layout.tsx` file, the URL will control the component output in `{children}`.

### Product Details Page

We will now add a [Next.js page](https://beta.nextjs.org/docs/routing/pages-and-layouts#pages) that gets the product ID from the URL and fetches/displays the details.

Create `app/[id]/page.tsx`. The `[id]` means we will pass down data from the URL as a prop named `id`:

```tsx
import Image from "next/image";
import { GetProductDocument } from "../../lib/gql/graphql";
import { gqlClient } from "../../lib/service/client";

async function getProduct(id: number) {
  const { product_by_pk } = await gqlClient.request(GetProductDocument, { id });
  return product_by_pk!;
}

export default async function Page({
  params: { id },
}: {
  params: { id: string };
}) {
  const product = await getProduct(Number(id));
  return (
    <>
      <h1>{product.name}</h1>
      <Image
        src={product.image_urls}
        alt={`${product.name} Picture`}
        width={200}
        height={200}
      ></Image>
      <div>{`$${product.price.toFixed(2)}`}</div>
      <p>{product.description}</p>
    </>
  );
}
```

Now when you click a link on the left, you should see the product details page appear on the right!

### Streaming and Suspense

[Streaming and Suspense are new React features](https://beta.nextjs.org/docs/data-fetching/fundamentals#streaming-and-suspense) that allow us to instantly display a loading UI, then stream in UI once data fetching is done.

As an example of this, create `app/loading.tsx`:

```tsx
export default function Loading() {
  // You can add any UI inside Loading, including a Skeleton.
  return <div>loading</div>;
}
```

Now when you try navigating between products you should see the loading UI before the details are loaded.

### Client Components

All the stuff we've built so far uses [Server Components](https://beta.nextjs.org/docs/rendering/server-and-client-components#server-components) which means they render on the server and don't need to ship JS to the browser.

However, sometimes we need to create [Client Components](https://beta.nextjs.org/docs/rendering/server-and-client-components#client-components) that have logic on the browser. For our example, we will have a button that orders a product and a [Hasura Streaming Subscription](https://hasura.io/docs/latest/subscriptions/postgres/streaming/index/) that shows us a real-time list of orders.

For our real-time subscription, we'll use the [graphql-ws](https://github.com/enisdenjo/graphql-ws) library. For the mutation we will continue to use `graphql-request`.

We create a new client component in `app/components/orders.tsx` using the syntax we are used to in React. The only new thing part is the `"use client";` at the top which tells Next.js this is a client component.

```tsx
"use client";

import { useEffect, useState } from "react";
import { PlaceOrderDocument } from "../../lib/gql/graphql";
import { createClient } from "graphql-ws";
import { gqlClient } from "../../lib/service/client";

const wsClient = createClient({
  url: process.env.NEXT_PUBLIC_HASURA_GRAPHQL_WS_URL!,
});

export default function Orders({ id }: { id: number }) {
  const [orders, setOrders] = useState<
    Array<{ id: number; created_at: string; quantity: number }>
  >([]);

  useEffect(() => {
    const unsubscribe = wsClient.subscribe(
      {
        query: `
          subscription ProductOrders($id: Int!) {
            order_product_stream(
              batch_size: 10
              cursor: { initial_value: { created_at: "2021-02-22T18:16:12.95499+00:00" } }
              where: { product_id: { _eq: $id } }
            ) {
              id
              quantity
              created_at
            }
          }`,
        variables: {
          id,
        },
      },
      {
        next: ({ data }) => {
          setOrders((orders) => [
            ...orders,
            ...(data?.order_product_stream as any),
          ]);
        },
        error: (error) => {
          console.log(error);
        },
        complete: () => {
          console.log("complete");
        },
      }
    );
    return () => {
      unsubscribe();
    };
  }, [id]);

  return (
    <>
      <button
        onClick={async () =>
          await gqlClient.request(PlaceOrderDocument, {
            products: { data: [{ product_id: id, quantity: 1 }] },
          })
        }
      >
        Order
      </button>
      <li>
        {orders.map((order) => (
          <li key={order.id}>
            {`Order ${order.id} created at ${new Date(order.created_at)}`}
          </li>
        ))}
      </li>
    </>
  );
}
```

When a new order comes from the subscription we update useState and render out the list of orders. When we click on the button, we trigger a GraphQL mutation to add a new order.

Now back in `app/[id]/page.tsx` we import the component and add it to the end of the page

```tsx
import Orders from "../components/orders";

export default async function Page({
  params: { id },
}: {
  params: { id: string };
}) {
  const product = await getProduct(Number(id));
  return (
    <>
      ...
      <Orders id={Number(id)}></Orders>
    </>
  );
}
```

When we navigate to the product detail page you should now be able to order a product and see new orders pop up in real-time!

## Conclusion

Next.js is packed with new features and deep integration with cutting-edge React techniques. Combined with Hasura, we can easily make great applications for our users!
