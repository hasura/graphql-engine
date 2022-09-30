export type FeatureFlagSections =
  | 'api'
  | 'data'
  | 'actions'
  | 'remote schemas'
  | 'events'
  | 'monitoring'
  | 'settings';

export type FeatureFlagStatus =
  | 'alpha'
  | 'beta'
  | 'release candidate'
  | 'stable'
  | 'experimental';

export type FeatureFlagId = string;

export interface FeatureFlagDefinition {
  id: FeatureFlagId;
  title: string;
  description: string;
  section: FeatureFlagSections;
  status: FeatureFlagStatus;
  discussionUrl: string;
  defaultValue: boolean;
}

export interface FeatureFlagState {
  id: FeatureFlagId;
  enabled: boolean;
  enableDate?: Date;
  dismissed: boolean;
  dismissedDate?: Date;
}

export interface FeatureFlagType extends FeatureFlagDefinition {
  state: Omit<FeatureFlagState, 'id'>;
}
