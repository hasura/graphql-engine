/* eslint-disable */
import * as types from './graphql';
import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';

const documents = {
    "query GetProducts {\n  product(limit: 10) {\n    id\n    name\n  }\n}\n\nquery GetProduct($id: Int!) {\n  product_by_pk(id: $id) {\n    id\n    description\n    name\n    price\n    image_urls(path: \"$[0]\")\n  }\n}\n\nmutation PlaceOrder($products: order_product_arr_rel_insert_input!) {\n  insert_order_one(\n    object: {products: $products, billing_address_id: 222, user_id: 225, shipping_address_id: 222}\n  ) {\n    id\n  }\n}": types.GetProductsDocument,
};

export function graphql(source: "query GetProducts {\n  product(limit: 10) {\n    id\n    name\n  }\n}\n\nquery GetProduct($id: Int!) {\n  product_by_pk(id: $id) {\n    id\n    description\n    name\n    price\n    image_urls(path: \"$[0]\")\n  }\n}\n\nmutation PlaceOrder($products: order_product_arr_rel_insert_input!) {\n  insert_order_one(\n    object: {products: $products, billing_address_id: 222, user_id: 225, shipping_address_id: 222}\n  ) {\n    id\n  }\n}"): (typeof documents)["query GetProducts {\n  product(limit: 10) {\n    id\n    name\n  }\n}\n\nquery GetProduct($id: Int!) {\n  product_by_pk(id: $id) {\n    id\n    description\n    name\n    price\n    image_urls(path: \"$[0]\")\n  }\n}\n\nmutation PlaceOrder($products: order_product_arr_rel_insert_input!) {\n  insert_order_one(\n    object: {products: $products, billing_address_id: 222, user_id: 225, shipping_address_id: 222}\n  ) {\n    id\n  }\n}"];

export function graphql(source: string): unknown;
export function graphql(source: string) {
  return (documents as any)[source] ?? {};
}

export type DocumentType<TDocumentNode extends DocumentNode<any, any>> = TDocumentNode extends DocumentNode<  infer TType,  any>  ? TType  : never;