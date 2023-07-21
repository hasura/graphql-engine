import * as z from 'zod';

/* 
 
  Credit for code from: https://github.com/colinhacks/zod/issues/372#issuecomment-1280054492

  implement() and type Implements allows you to construct a zod schema that points back to an existing typescript type.

  Like this:

  type User = {
    name: string;
    age: number;
    dob: Date;
    favoriteColor: 'blue' | 'red';
  };
  const schema = implement<User>().with({
    name: z.string(),
    age: z.number(),
    dob: z.date(),
    favoriteColor: z.enum(['blue', 'red']),
  });
  
  If anything in `schema` does not correctly represent User, then TS will detect an error.

  NOTE from author:

  The only thing not supported is .nullable().optional() 🤷‍♂️ It must be .optional().nullable() or .nullish()
*/

type Implements<Model> = {
  [key in keyof Model]-?: undefined extends Model[key]
    ? null extends Model[key]
      ? z.ZodNullableType<z.ZodOptionalType<z.ZodType<Model[key]>>>
      : z.ZodOptionalType<z.ZodType<Model[key]>>
    : null extends Model[key]
    ? z.ZodNullableType<z.ZodType<Model[key]>>
    : z.ZodType<Model[key]>;
};

export function implement<Model = never>() {
  return {
    with: <
      Schema extends Implements<Model> & {
        [unknownKey in Exclude<keyof Schema, keyof Model>]: never;
      }
    >(
      schema: Schema
    ) => z.object(schema),
  };
}

export const reqString = (name: string) => {
  return z
    .string({ required_error: `${name} is required` })
    .trim()
    .min(1, `${name} is required`);
};
