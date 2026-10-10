import * as z from 'zod';

export const reqString = (name: string) => {
  return z
    .string({ error: `${name} is required` })
    .trim()
    .min(1, `${name} is required`);
};
