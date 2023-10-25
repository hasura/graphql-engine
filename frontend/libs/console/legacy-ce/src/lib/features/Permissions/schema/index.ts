import * as z from 'zod';
import { inputValidationSchema } from '../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';

const columns = z.record(z.optional(z.boolean()));
const presets = z.optional(
  z.array(
    z.object({
      columnName: z.string(),
      presetType: z.optional(z.string()),
      columnValue: z.optional(z.union([z.string(), z.number()])),
    })
  )
);

export type Presets = z.infer<typeof presets>;

export type PermissionsSchema = z.infer<typeof schema>;

const queryType = z.union([
  z.literal(''),
  z.literal('insert'),
  z.literal('select'),
  z.literal('update'),
  z.literal('delete'),
]);

export type Permission = z.infer<typeof permission>;

const permission = z.object({
  tableName: z.string(),
  queryType,
  roleName: z.string(),
});

export const schema = z.discriminatedUnion('queryType', [
  z.object({
    queryType: z.literal('insert'),
    checkType: z.string(),
    filterType: z.string(),
    comment: z.string(),
    check: z.any(),
    columns,
    presets,
    backendOnly: z.boolean().optional(),
    supportedOperators: z.array(z.any()),
    clonePermissions: z.array(permission).optional(),
    validateInput: inputValidationSchema.optional(),
  }),
  z.object({
    queryType: z.literal('select'),
    filterType: z.string(),
    comment: z.string(),
    filter: z.any(),
    columns,
    presets,
    rowCount: z.string().optional(),
    aggregationEnabled: z.boolean().optional(),
    clonePermissions: z.array(permission).optional(),
    query_root_fields: z.array(z.string()).nullable().optional(),
    subscription_root_fields: z.array(z.string()).nullable().optional(),
    supportedOperators: z.array(z.any()),
    validateInput: inputValidationSchema.optional(),
  }),
  z.object({
    queryType: z.literal('update'),
    columns,
    filterType: z.string(),
    comment: z.string(),
    filter: z.any(),
    checkType: z.string(),
    check: z.any(),
    presets,
    backendOnly: z.boolean().optional(),
    supportedOperators: z.array(z.any()),
    clonePermissions: z.array(permission).optional(),
    validateInput: inputValidationSchema.optional(),
  }),
  z.object({
    queryType: z.literal('delete'),
    filterType: z.string(),
    comment: z.string(),
    filter: z.any(),
    backendOnly: z.boolean().optional(),
    supportedOperators: z.array(z.any()),
    clonePermissions: z.array(permission).optional(),
    validateInput: inputValidationSchema.optional(),
  }),
]);

// Values from Hasura docs
// link: https://hasura.io/docs/latest/api-reference/syntax-defs/#insertpermission

// Insert
// check	true	BoolExp	This expression has to hold true for every new row that is inserted
// set	false	ColumnPresetsExp	Preset values for columns that can be sourced from session variables or static values
// columns	false	PGColumn array (or) '*'	Can insert into only these columns (or all when '*' is specified)
// backend_only	false	Boolean	When set to true the mutation is accessible only if the x-hasura-use-backend-only-permissions session variable exists and is set to true and the request is made with x-hasura-admin-secret set if any auth is configured

// Select
// columns	true	PGColumn array (or) '*'	Only these columns are selectable (or all when '*' is specified)
// computed_fields	false	ComputedFieldName array	Only these computed fields are selectable
// filter	true	BoolExp	Only the rows where this expression holds true are selectable
// limit	false	Integer	The maximum number of rows that can be returned
// allow_aggregations	false	Boolean	Toggle allowing aggregate queries
// query_root_fields	false	QueryRootField array	Only given root fields will be enabled in the query root field. An empty list will mean no query root fields are enabled .
// subscription_root_fields	false	SubscriptionRootField array	Only given root fields will be enabled in the subscription root field. An empty list will mean no subscription root fields are enabled.

// Update
// columns	true	PGColumn array (or) '*'	Only these columns are selectable (or all when '*' is specified)
// filter	true	BoolExp	Only the rows where this precondition holds true are updatable
// check	false	BoolExp	Postcondition which must be satisfied by rows which have been updated
// set	false	ColumnPresetsExp	Preset values for columns that can be sourced from session variables or static values.
// backend_only	false	Boolean	When set to true the mutation is accessible only if the x-hasura-use-backend-only-permissions session variable exists and is set to true and the request is made with x-hasura-admin-secret set if any auth is configured

// Delete
// filter	true	BoolExp	Only the rows where this expression holds true are deletable
// backend_only	false	Boolean	When set to true the mutation is accessible only if the x-hasura-use-backend-only-permissions session variable exists and is set to true and the request is made with x-hasura-admin-secret set if any auth is configured
