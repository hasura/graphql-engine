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
