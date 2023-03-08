import { z } from 'zod';

// This type represents the part of the diagnostic file used by the generator
export const chromaticDiagnosticFileSchema = z.object({
  build: z.object({
    // see: https://www.chromatic.com/docs/integrations#result-and-status-codes
    status: z.union([
      z.literal('PENDING'),
      z.literal('PASSED'),
      z.literal('ACCEPTED'),
      z.literal('DENIED'),
      z.literal('BROKEN'),
      z.literal('CANCELLED'),
      z.literal('FAILED'),
    ]),

    changeCount: z.number(),
    errorCount: z.number(),
    webUrl: z.string().url().optional(),
  }),
});

export type ChromaticDiagnosticFile = z.infer<
  typeof chromaticDiagnosticFileSchema
>;
