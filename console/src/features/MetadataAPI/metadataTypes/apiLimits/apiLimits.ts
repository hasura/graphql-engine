import { Nullable } from '@/components/Common/utils/tsUtils';

type APILimit<T> = {
  global: T;
  per_role?: Record<string, T>;
};

export type ApiLimits = {
  disabled?: boolean;
  depth_limit?: APILimit<number>;
  node_limit?: APILimit<number>;
  time_limit?: APILimit<number>;
  batch_limit?: APILimit<number>;
  rate_limit?: APILimit<{
    unique_params: Nullable<'IP' | string[]>;
    max_reqs_per_min: number;
  }>;
};
