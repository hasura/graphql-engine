import { rest } from 'msw';
import {
  mockInconsistentMetadata,
  mockListSourceKindsResponse,
  mockMetadata,
} from './mockData';
import { extractTypeAndArgs } from '../../../Data/LogicalModels/AddNativeQuery/mocks/native-query-handlers';

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, async (_req, res, ctx) => {
    const { type } = await extractTypeAndArgs(_req);

    if (type === 'list_source_kinds')
      return res(ctx.json(mockListSourceKindsResponse));
    if (type === 'export_metadata') return res(ctx.json(mockMetadata));
    if (type === 'get_inconsistent_metadata')
      return res(ctx.json(mockInconsistentMetadata));
  }),
];
