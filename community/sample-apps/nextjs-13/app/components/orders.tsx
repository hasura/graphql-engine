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
