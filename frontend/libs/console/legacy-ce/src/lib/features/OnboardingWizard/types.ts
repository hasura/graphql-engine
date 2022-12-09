export type UserOnboarding = {
  activity: Record<string, any>;
  target: string;
};

export type OnboardingResponseData = {
  data: {
    user_onboarding: UserOnboarding[];
  };
};
