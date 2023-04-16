export type InconsistentObject = {
  definition: any;
  reason: string;
  type: string;
  name: string;
  message?: string;
};

export type InconsistentMetadata = {
  inconsistent_objects: InconsistentObject[];
  is_consistent: boolean;
};
