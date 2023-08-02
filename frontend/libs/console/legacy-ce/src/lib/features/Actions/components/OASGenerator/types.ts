import { RequestTransformMethod } from '../../../../metadata/types';
import { createGraphQLSchema } from '@hasura/open-api-to-graphql';
import z from 'zod';
import { formSchema } from './OASGeneratorPage';

export type SchemaType = z.infer<typeof formSchema>;

export type RequestTransform =
  | {
      type: 'json';
      value: string;
    }
  | {
      type: 'x-www-form-urlencoded';
      value: Record<string, string>;
    };

export type GeneratedAction = {
  operationId: string;
  actionType: 'query' | 'mutation';
  types: string;
  action: string;
  description: string;
  method: RequestTransformMethod;
  baseUrl: string;
  path: string;
  requestTransforms?: RequestTransform;
  responseTransforms: string;
  sampleInput: string;
  headers: string[];
  queryParams: string | { name: string; value: string }[];
};

export type Result = Awaited<ReturnType<typeof createGraphQLSchema>>;
export type Operation = Result['data']['operations'][0];

export type DataDefinition = Operation['responseDefinition'];
export type SubDefinition = Operation['responseDefinition']['subDefinitions'];

export type OperationParameters = Operation['parameters'];

export interface OASError extends Error {
  message: string;
  options: {
    context: string[];
  };
}
