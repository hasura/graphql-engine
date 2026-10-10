import { dataQueryTypeSchema } from '@hasura/shared/types';
import z from 'zod';

export type ClonePermissionItem = z.infer<typeof permission>;

const permission = z.object({
  table: z.unknown(),
  queryType: dataQueryTypeSchema,
  roleName: z.string(),
});

export const clonePermissionsSchema = z.array(permission);

export type ClonePermissionSchema = z.infer<typeof clonePermissionsSchema>;
