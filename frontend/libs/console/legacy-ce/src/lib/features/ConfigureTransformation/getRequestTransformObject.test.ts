import { getRequestTransformObject } from './utils';
import { requestTransformState } from './requestTransformState';
import { RequestTransformState } from './stateDefaults';

// Regression coverage for the query-param serialisation in
// `getRequestTransformObject`: the key/value editor keeps a trailing empty-name
// placeholder row, which previously serialised as a junk `{ "": "" }`. Empty-name
// pairs must be dropped (an untouched transform -> `{}`), while real entries —
// including a named key with an empty value — must be preserved, and a raw string
// (Kriti template) query-param must pass through untouched.
const urlTransform = (
  requestQueryParams: RequestTransformState['requestQueryParams'],
): RequestTransformState => ({
  ...requestTransformState,
  isRequestUrlTransform: true,
  isRequestPayloadTransform: false,
  requestMethod: 'GET',
  requestUrl: '/users',
  requestQueryParams,
});

describe('getRequestTransformObject query_params serialisation', () => {
  it('drops the untouched empty-name placeholder row (serialises {})', () => {
    const obj = getRequestTransformObject(
      urlTransform([{ name: '', value: '' }]),
    );
    expect(obj?.query_params).toEqual({});
  });

  it('retains a named key with an empty value', () => {
    const obj = getRequestTransformObject(
      urlTransform([
        { name: 'id', value: '' },
        { name: '', value: '' },
      ]),
    );
    expect(obj?.query_params).toEqual({ id: '' });
  });

  it('retains named parameters (and drops only the trailing placeholder)', () => {
    const obj = getRequestTransformObject(
      urlTransform([
        { name: 'id', value: '5' },
        { name: 'name', value: 'login' },
        { name: '', value: '' },
      ]),
    );
    expect(obj?.query_params).toEqual({ id: '5', name: 'login' });
  });

  it('passes a raw string (Kriti template) query-param through untouched', () => {
    const obj = getRequestTransformObject(urlTransform('userId={{$user.id}}'));
    expect(obj?.query_params).toEqual('userId={{$user.id}}');
  });
});
