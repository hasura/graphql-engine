export type APILimit<T> = {
  global: T;
  per_role?: Record<string, T>;
  state: 'disabled' | 'enabled' | 'global';
};

export type RateLimit = APILimit<{
  unique_params?: 'IP' | string[] | null;
  max_reqs_per_min?: number;
}>;

export type ApiLimits = {
  disabled?: boolean;
  depth_limit?: APILimit<number>;
  node_limit?: APILimit<number>;
  time_limit?: APILimit<number>;
  batch_limit?: APILimit<number>;
  rate_limit?: RateLimit;
};
