import { OnboardingKind, FallbackApp } from '../types';

export const useFallbackApps = (kind: OnboardingKind): FallbackApp[] => {
  if (kind.kind === 'one-click-deployment') {
    return kind.fallbackApps
      .map(
        ({
          git_repository_branch,
          rank,
          git_repository_url,
          name,
          hasura_directory,
          react_icons_fa_component_name,
        }) => {
          return {
            react_icons_component_name: react_icons_fa_component_name || '',
            name,
            href: `/deploy?github_repo=${git_repository_url}&hasura_dir=${hasura_directory}&branch=${git_repository_branch}`,
            rank,
          };
        }
      )
      .sort(({ rank: rank1 }, { rank: rank2 }) => {
        if (rank1 > rank2) return 1;
        if (rank1 < rank2) return -1;
        return 0;
      });
  }
  return [];
};
