import { GraphQLInputField } from 'graphql';
import { ReactText } from 'react';
import { rspInputEffect } from './RSPInput';

describe('rspInputEffect', () => {
  describe('when type is Int and localValue is set and is a numeric string not 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = jest.fn();
      const v: GraphQLInputField = {
        name: 'foo',
        extensions: [],
        type: {
          inspect: () => 'Int',
          toJSON: () => '1',
          ofType: '',
        },
      };

      const localValue: ReactText = '1';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: Number(localValue) });
    });
  });

  describe('when type is Int! and localValue is set and is a numeric string equals to 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = jest.fn();
      const v: GraphQLInputField = {
        name: 'foo',
        extensions: [],
        type: {
          inspect: () => 'Int!',
          toJSON: () => '0',
          ofType: '',
        },
      };

      const localValue: ReactText = '0';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: 0 });
    });
  });

  describe('when type is not Int and localValue is set and is a numeric string equals to 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = jest.fn();
      const v: GraphQLInputField = {
        name: 'foo',
        extensions: [],
        type: {
          inspect: () => 'String',
          toJSON: () => '0',
          ofType: '',
        },
      };

      const localValue: ReactText = 'text';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: 'text' });
    });
  });
});
