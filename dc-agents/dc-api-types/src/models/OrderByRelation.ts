/* generated using openapi-typescript-codegen -- do not edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { Expression } from './Expression';
export type OrderByRelation = {
  /**
   * Further relationships to follow from the relationship's target table. The key of the map is the relationship name.
   */
  subrelations: Record<string, OrderByRelation>;
  where?: Expression;
};

