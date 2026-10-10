import { getRemoteFieldPath } from '../utils';

describe('getRemoteFieldPath', () => {
  it('should work with simple remote field', () => {
    const res = getRemoteFieldPath({ continents: { arguments: {} } });
    expect(res).toMatchSnapshot();
  });
  it('should work with long remote field', () => {
    const res = getRemoteFieldPath({
      country: {
        field: {
          continent: {
            field: {
              countries: {
                field: {
                  states: {
                    arguments: {},
                  },
                },
                arguments: {},
              },
            },
            arguments: {},
          },
        },
        arguments: {},
      },
    });
    expect(res).toMatchSnapshot();
  });
});
