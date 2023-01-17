export const simpleExample = {
  Title: {
    _eq: 'hello',
  },
};

export const exampleWithBoolOperator = {
  _and: [
    {
      age: {
        _eq: 8,
      },
    },
    {
      email: {
        _eq: 'adsff',
      },
    },
  ],
};

export const exampleWithRelationship = {
  things: {
    name: {
      _eq: 'asdas',
    },
  },
};

export const complicatedExample = {
  _and: [
    {
      things: {
        _and: [
          {
            user: {
              age: {
                _eq: 22,
              },
            },
          },
          {
            _or: [
              {
                fk_user_id: {
                  _eq: 'X-Hasura-User-Id',
                },
              },
              {
                user: {
                  age: {
                    _gte: 44,
                  },
                },
              },
            ],
          },
        ],
      },
    },
  ],
};
