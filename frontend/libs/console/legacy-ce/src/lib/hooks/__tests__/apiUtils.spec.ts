import 'whatwg-fetch';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { Api } from '../apiUtils';
import { APIError } from '../error';

const server = setupServer();

const url = 'https://example.com/mock';
const headers = {
  'Content-Type': 'application/json',
};

const REST_BODY = {
  stringValue: 'toto',
  numberValue: 1,
  array: ['item 1', 'item 2', 'item 3'],
  object: {
    hello: 'world',
  },
};

const STRING_BODY = 'some body';

const MOCK_POST_BODY = {
  key: 'data',
  value: 2,
};

const MOCK_ERROR_RESPONSE_STANDARD = {
  message: 'Something bad appended, please try again',
};

const MOCK_ERROR_STRING = 'Some error we got.';

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

describe('API functions', () => {
  describe('successful requests', () => {
    it('should allow for posting data', async () => {
      let dataFromServer;
      server.use(
        rest.post(url, (req, res, context) => {
          dataFromServer = req.body;
          return res(context.json(REST_BODY), context.status(200));
        })
      );

      const response = await Api.post<typeof REST_BODY>({
        url,
        body: MOCK_POST_BODY,
        headers,
      });
      expect(response).toEqual(REST_BODY);
      expect(dataFromServer).toEqual(MOCK_POST_BODY);
    });
    describe('and the response is json', () => {
      it('should dispatch the DONE_REQUEST when the request is done', async () => {
        server.use(
          rest.get(url, (_req, res, context) => {
            return res(context.json(REST_BODY), context.status(200));
          })
        );
        const response = Api.get({ url, headers });
        await expect(response).resolves.toStrictEqual(REST_BODY);
      });
      it('should work the the content type have a charset to it', async () => {
        server.use(
          rest.get(url, (_req, res, context) => {
            return res(
              context.body(JSON.stringify(REST_BODY)),
              context.set('Content-Type', 'application/json; charset=utf-8'),
              context.status(200)
            );
          })
        );
        const response = Api.get({ url, headers });
        await expect(response).resolves.toStrictEqual(REST_BODY);
      });
      it('should allow a transform function', async () => {
        server.use(
          rest.get(url, (_req, res, context) => {
            return res(context.json(REST_BODY), context.status(200));
          })
        );
        const response = Api.get<typeof REST_BODY, Record<string, string>>(
          { url, headers },
          b => b.object
        );
        await expect(response).resolves.toStrictEqual(REST_BODY.object);
      });
    });
    describe('and the response is raw text', () => {
      it('should work as expected', async () => {
        server.use(
          rest.get(url, (_req, res, context) => {
            return res(context.text(STRING_BODY), context.status(200));
          })
        );
        const response = Api.get({ url, headers });
        await expect(response).resolves.toStrictEqual(STRING_BODY);
      });
    });
  });
  describe('error cases', () => {
    [400, 401, 403, 404, 408, 500, 524].forEach(errorCode => {
      describe(`when an error ${errorCode} occurs`, () => {
        describe('and the response is json', () => {
          it('should throw the correct error', async () => {
            server.use(
              rest.get(url, (_req, res, context) => {
                return res(
                  context.json(MOCK_ERROR_RESPONSE_STANDARD),
                  context.status(errorCode)
                );
              })
            );
            const response = Api.get({ url, headers });
            await expect(response).rejects.toStrictEqual(
              new APIError(MOCK_ERROR_RESPONSE_STANDARD.message)
            );
          });
          it('should handle unexpected error object', async () => {
            server.use(
              rest.get(url, (_req, res, context) => {
                return res(
                  context.json(REST_BODY.object),
                  context.status(errorCode)
                );
              })
            );
            const response = Api.get({ url, headers });
            await expect(response).rejects.toStrictEqual(
              new APIError(JSON.stringify(REST_BODY.object))
            );
          });
          it('should not call transFrom function on Error', async () => {
            server.use(
              rest.get(url, (_req, res, context) => {
                return res(
                  context.json(MOCK_ERROR_RESPONSE_STANDARD),
                  context.status(errorCode)
                );
              })
            );
            const response = Api.get<{ invalid: string }, string>(
              { url, headers },
              d => d.invalid
            );
            await expect(response).rejects.toStrictEqual(
              new APIError(MOCK_ERROR_RESPONSE_STANDARD.message)
            );
          });
        });
        describe('and the response is raw text', () => {
          it('should throw the correct error', async () => {
            server.use(
              rest.get(url, (_req, res, context) => {
                return res(
                  context.text(MOCK_ERROR_STRING),
                  context.status(errorCode)
                );
              })
            );
            const response = Api.get({ url, headers });
            await expect(response).rejects.toStrictEqual(
              new APIError(MOCK_ERROR_STRING)
            );
          });
        });
      });
    });

    describe('errorTransform', () => {
      it('when errorTransform is passed, should call the errorTransform function with the raw error', async () => {
        server.use(
          rest.get(url, (_req, res, context) => {
            // Make every request failing with { foo: 'bar' }
            return res(context.json({ foo: 'bar' }), context.status(400));
          })
        );

        const errorTransform = jest.fn(error => error);
        const promise = Api.get({ url, headers }, undefined, errorTransform);

        await expect(promise).rejects.toEqual({ foo: 'bar' });
        expect(errorTransform).toBeCalledWith({ foo: 'bar' });
      });
    });
  });
});
