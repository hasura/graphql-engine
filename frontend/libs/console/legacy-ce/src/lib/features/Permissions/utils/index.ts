import {
  keyToPermission,
  metadataPermissionKeys,
  TablePermission,
} from '@hasura/shared/types';

export const isPermission = (props: {
  key: string;
  value: any;
}): props is {
  key: (typeof metadataPermissionKeys)[number];
  value: TablePermission[];
} => props.key in keyToPermission;
