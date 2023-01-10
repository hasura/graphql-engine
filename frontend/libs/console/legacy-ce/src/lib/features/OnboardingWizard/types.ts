export type UserOnboarding = {
  activity: Record<string, any>;
  target: string;
};

export type User = {
  id: string;
  created_at: string;
};

export type OnboardingResponseData = {
  data: {
    user_onboarding: UserOnboarding[];
    users: User[];
  };
};
