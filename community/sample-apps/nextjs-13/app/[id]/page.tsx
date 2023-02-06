import Image from "next/image";
import { GetProductDocument } from "../../lib/gql/graphql";
import { gqlClient } from "../../lib/service/client";
import Orders from "../components/orders";

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
      <Orders id={Number(id)}></Orders>
    </>
  );
}
