import { rest } from 'msw';
import {
  metadata,
  metadataWithExistingRelationships,
  query,
} from './dataStubs';

const baseUrl = 'http://localhost:8080';

interface HandlerProps {
  url?: string;
  withExisting?: boolean;
}

export const handlers = (args?: HandlerProps) => [
  rest.post(`${args?.url || baseUrl}/v2/query`, (_req, res, ctx) =>
    res(ctx.json(query))
  ),

  rest.post(`${args?.url || baseUrl}/v1/metadata`, (_req, res, ctx) => {
    if (args?.withExisting) {
      return res(ctx.json(metadataWithExistingRelationships));
    }

    return res(ctx.json(metadata));
  }),
];
