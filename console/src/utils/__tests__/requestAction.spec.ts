import { setupServer } from 'msw/node';
import thunk from 'redux-thunk';
import configureStore from 'redux-mock-store';
import { rest } from 'msw';
import {
  DONE_REQUEST,
  ERROR_REQUEST,
  FAILED_REQUEST,
  LOAD_REQUEST,
} from '../../components/App/Actions';
import requestAction from '../requestAction';
import { Dispatch, ReduxState } from '../../types';

const middlewares = [thunk];
const mockStore = configureStore<Partial<ReduxState>, Dispatch>(middlewares);

const server = setupServer();

const MOCK_URL = 'https://example.com/mock';

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
  error: 'Something bad appended, please try again',
};

const MOCK_ERROR_STRING = 'Some error we got.';
const SUCCESS_EVENT_NAME = 'mock/SUCCESS';
const FAILURE_EVENT_NAME = 'mock/FAILURE';

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

describe('requestAction', () => {
  describe('successful requests', () => {
    it('should allow for posting data', async () => {
      let dataFromServer;
      server.use(
        rest.post(MOCK_URL, (req, res, context) => {
          dataFromServer = req.body;
          return res(context.json(REST_BODY), context.status(200));
        })
      );
      const expectedActions = [
        { type: LOAD_REQUEST },
        { type: SUCCESS_EVENT_NAME, data: REST_BODY },
        { type: DONE_REQUEST },
      ];
      const store = mockStore({});

      await store.dispatch(
        requestAction(
          MOCK_URL,
          {
            method: 'POST',
            body: JSON.stringify(MOCK_POST_BODY),
            headers: {
              'Content-Type': 'application/json',
            },
          },
          SUCCESS_EVENT_NAME,
          FAILURE_EVENT_NAME
        )
      );
      expect(store.getActions()).toEqual(expectedActions);
      expect(dataFromServer).toEqual(MOCK_POST_BODY);
    });
    describe('and the response is json', () => {
      it('should dispatch the DONE_REQUEST when the request is done', async () => {
        server.use(
          rest.get(MOCK_URL, (req, res, context) => {
            return res(context.json(REST_BODY), context.status(200));
          })
        );
        const expectedActions = [
          { type: LOAD_REQUEST },
          { type: DONE_REQUEST },
        ];
        const store = mockStore({});

        await expect(
          store.dispatch(requestAction(MOCK_URL))
        ).resolves.toStrictEqual(REST_BODY);
        expect(store.getActions()).toEqual(expectedActions);
      });
      it('should work the the content type have a charset to it', async () => {
        server.use(
          rest.get(MOCK_URL, (req, res, context) => {
            return res(
              context.body(JSON.stringify(REST_BODY)),
              context.set('Content-Type', 'application/json; charset=utf-8'),
              context.status(200)
            );
          })
        );
        const expectedActions = [
          { type: LOAD_REQUEST },
          { type: DONE_REQUEST },
        ];
        const store = mockStore({});

        await expect(
          store.dispatch(requestAction(MOCK_URL))
        ).resolves.toStrictEqual(REST_BODY);
        expect(store.getActions()).toEqual(expectedActions);
      });
      it('should allow for a custom success event', async () => {
        server.use(
          rest.get(MOCK_URL, (req, res, context) => {
            return res(context.json(REST_BODY), context.status(200));
          })
        );
        const expectedActions = [
          { type: LOAD_REQUEST },
          { type: SUCCESS_EVENT_NAME, data: REST_BODY },
          { type: DONE_REQUEST },
        ];
        const store = mockStore({});

        await expect(
          store.dispatch(
            requestAction(MOCK_URL, {}, SUCCESS_EVENT_NAME, FAILURE_EVENT_NAME)
          )
        ).resolves.toStrictEqual(REST_BODY);
        expect(store.getActions()).toEqual(expectedActions);
      });
    });
    describe('and the response is raw text', () => {
      it('should dispatch the DONE_REQUEST when the request is done', async () => {
        server.use(
          rest.get(MOCK_URL, (req, res, context) => {
            return res(context.text(STRING_BODY), context.status(200));
          })
        );
        const expectedActions = [
          { type: LOAD_REQUEST },
          { type: DONE_REQUEST },
        ];
        const store = mockStore({});

        await expect(
          store.dispatch(requestAction(MOCK_URL))
        ).resolves.toStrictEqual(STRING_BODY);
        expect(store.getActions()).toEqual(expectedActions);
      });
      it('should allow for a custom success event', async () => {
        server.use(
          rest.get(MOCK_URL, (req, res, context) => {
            return res(context.text(STRING_BODY), context.status(200));
          })
        );
        const expectedActions = [
          { type: LOAD_REQUEST },
          { type: SUCCESS_EVENT_NAME, data: STRING_BODY },
          { type: DONE_REQUEST },
        ];
        const store = mockStore({});

        await expect(
          store.dispatch(
            requestAction(MOCK_URL, {}, SUCCESS_EVENT_NAME, FAILURE_EVENT_NAME)
          )
        ).resolves.toStrictEqual(STRING_BODY);
        expect(store.getActions()).toEqual(expectedActions);
      });
    });
  });
  describe('error cases', () => {
    [400, 401, 403, 404, 408, 500].forEach(errorCode => {
      describe(`when an error ${errorCode} occurs`, () => {
        describe('and the response is json', () => {
          it('should dispatch the ERROR_REQUEST with the payload of the request when no dispatch name is provided', async () => {
            server.use(
              rest.get(MOCK_URL, (req, res, context) => {
                return res(
                  context.json(MOCK_ERROR_RESPONSE_STANDARD),
                  context.status(errorCode)
                );
              })
            );
            const expectedActions = [
              { type: LOAD_REQUEST },
              { type: FAILED_REQUEST },
              {
                type: ERROR_REQUEST,
                data: MOCK_ERROR_RESPONSE_STANDARD,
                params: undefined,
                statusCode: errorCode,
                url: MOCK_URL,
              },
            ];
            const store = mockStore({});

            await expect(
              store.dispatch(requestAction(MOCK_URL))
            ).rejects.toStrictEqual(MOCK_ERROR_RESPONSE_STANDARD);
            expect(store.getActions()).toEqual(expectedActions);
          });
          it('should dispatch the error parameter when is supplied for rest output', async () => {
            server.use(
              rest.get(MOCK_URL, (req, res, context) => {
                return res(
                  context.json(MOCK_ERROR_RESPONSE_STANDARD),
                  context.status(errorCode)
                );
              })
            );
            const expectedActions = [
              { type: LOAD_REQUEST },
              { type: FAILED_REQUEST },
              {
                type: FAILURE_EVENT_NAME,
                data: MOCK_ERROR_RESPONSE_STANDARD,
              },
            ];
            const store = mockStore({});

            await expect(
              store.dispatch(
                requestAction(
                  MOCK_URL,
                  {},
                  SUCCESS_EVENT_NAME,
                  FAILURE_EVENT_NAME
                )
              )
            ).rejects.toStrictEqual(MOCK_ERROR_RESPONSE_STANDARD);
            expect(store.getActions()).toEqual(expectedActions);
          });
        });
        describe('and the response is raw text', () => {
          it('should dispatch the ERROR_REQUEST with the payload of the request when no dispatch name is provided', async () => {
            server.use(
              rest.get(MOCK_URL, (req, res, context) => {
                return res(
                  context.text(MOCK_ERROR_STRING),
                  context.status(errorCode)
                );
              })
            );
            const expectedActions = [
              { type: LOAD_REQUEST },
              { type: FAILED_REQUEST },
              {
                type: ERROR_REQUEST,
                data: MOCK_ERROR_STRING,
                params: undefined,
                statusCode: errorCode,
                url: MOCK_URL,
              },
            ];
            const store = mockStore({});

            await expect(
              store.dispatch(requestAction(MOCK_URL))
            ).rejects.toStrictEqual(MOCK_ERROR_STRING);
            expect(store.getActions()).toEqual(expectedActions);
          });
          it('should dispatch the error parameter when is supplied for rest output', async () => {
            server.use(
              rest.get(MOCK_URL, (req, res, context) => {
                return res(
                  context.text(MOCK_ERROR_STRING),
                  context.status(errorCode)
                );
              })
            );
            const expectedActions = [
              { type: LOAD_REQUEST },
              { type: FAILED_REQUEST },
              {
                type: FAILURE_EVENT_NAME,
                data: MOCK_ERROR_STRING,
              },
            ];
            const store = mockStore({});

            await expect(
              store.dispatch(
                requestAction(
                  MOCK_URL,
                  {},
                  SUCCESS_EVENT_NAME,
                  FAILURE_EVENT_NAME
                )
              )
            ).rejects.toStrictEqual(MOCK_ERROR_STRING);
            expect(store.getActions()).toEqual(expectedActions);
          });
        });
      });
    });
  });
});
