import React from 'react';
import { HasuraOnboarding } from './HasuraOnboardingSVG';

export function OnboardingAnimation() {
  return (
    <div className="flex flex-col justify-center items-center overflow-auto bg-gray-200 border border-gray-300 rounded-b p-md mb-md">
      <div>
        <HasuraOnboarding />
      </div>
      <div className="font-normal text-sm text-gray-600 mt-md">
        We recommend connecting a database to instantly explore the
        auto-generated <b>Hasura GraphQL API</b>
      </div>
    </div>
  );
}
