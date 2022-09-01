import { rest } from 'msw';
import { TMigration } from '../../../../MetadataAPI/hooks/useMetadataMigration';

import {
  alreadyExistsResponse,
  createMetadata,
  notExistsResponse,
  configApiEnabled,
} from './mockData';

export const handlers = (delay = 0, url = 'http://localhost:8080') => {
  const metadata = createMetadata();

  return [
    rest.get(`${url}/v1alpha1/config`, (req, res, ctx) => {
      return res(ctx.delay(delay), ctx.status(200), ctx.json(configApiEnabled));
    }),

    rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
      const body = req.body as TMigration['query'];

      switch (body?.type) {
        case 'export_metadata':
          return res(ctx.delay(delay), ctx.json(metadata));

        case 'bulk':
          const name = body?.args?.[0]?.args?.name;
          if (
            (metadata.metadata.query_collections || []).find(
              c => c.name === name
            )
          ) {
            return res(
              ctx.delay(delay),
              ctx.status(400),
              ctx.json(alreadyExistsResponse(name))
            );
          }
          (metadata.metadata.query_collections || []).push({
            name,
            definition: {
              queries: [],
            },
          });
          return res(ctx.delay(delay), ctx.json(metadata));

        case 'drop_query_collection':
          const collection = body.args.collection;
          if (
            !(metadata.metadata.query_collections || []).find(
              c => c.name === collection
            )
          ) {
            return res(
              ctx.delay(delay),
              ctx.status(400),
              ctx.json(notExistsResponse(collection))
            );
          }
          metadata.metadata.query_collections = (
            metadata.metadata.query_collections || []
          ).filter(c => c.name !== collection);
          return res(ctx.delay(delay), ctx.json(metadata));

        case 'rename_query_collection':
          const fromName = body?.args?.name;
          if (
            !(metadata.metadata.query_collections || []).find(
              c => c.name === fromName
            )
          ) {
            return res(
              ctx.delay(delay),
              ctx.status(400),
              ctx.json(notExistsResponse(fromName))
            );
          }
          metadata.metadata.query_collections = (
            metadata.metadata.query_collections || []
          ).map(c =>
            c.name === fromName ? { ...c, name: body.args.new_name } : c
          );

          return res(ctx.delay(delay), ctx.json(metadata));

        default:
          return res(ctx.delay(delay), ctx.status(500));
      }
    }),
  ];
};
