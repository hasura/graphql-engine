import { GraphQLInputField, GraphQLNonNull, GraphQLScalarType } from 'graphql';
import React from 'react';
import { vi } from 'vitest';
import { rspInputEffect } from './RSPInput';

describe('rspInputEffect', () => {
  describe('when type is Int and localValue is set and is a numeric string not 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = vi.fn();
      const v = {
        name: 'foo',
        type: new GraphQLScalarType({
          name: 'Int',
        }),
      } as GraphQLInputField;

      const localValue: React.ReactNode = '1';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: Number(localValue) });
    });
  });

  describe('when type is Int! and localValue is set and is a numeric string equals to 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = vi.fn();
      const v: GraphQLInputField = {
        name: 'foo',
        type: new GraphQLNonNull(
          new GraphQLScalarType({
            name: 'Int',
          }),
        ),
      } as GraphQLInputField;

      const localValue: React.ReactNode = '0';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: 0 });
    });
  });

  describe('when type is not Int and localValue is set and is a numeric string equals to 0', () => {
    it('invokes setArgVal', () => {
      const setArgVal = vi.fn();
      const v = {
        name: 'foo',
        type: new GraphQLScalarType({
          name: 'String',
        }),
      } as GraphQLInputField;

      const localValue: React.ReactNode = 'text';

      rspInputEffect({ v, localValue, setArgVal });

      expect(setArgVal).toHaveBeenCalledWith({ foo: 'text' });
    });
  });
});
