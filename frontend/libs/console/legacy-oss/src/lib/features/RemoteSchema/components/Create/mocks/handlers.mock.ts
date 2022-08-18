import { rest } from 'msw';

export const handlers = () => [
  rest.post('http://localhost:8080/v1/metadata', (req, res, ctx) => {
    return res(ctx.json({ message: 'success' }));
  }),
];
