import React from 'react';
import { IconCardGroup, IconCardGroupItem } from './IconCardGroup';

type HasuraFamiliaritySurveyProps = {
  data: { question: string; options: IconCardGroupItem<string>[] };
  onOptionClick: (optionValue: string) => void;
  onSkip?: () => void;
};
export function HasuraFamiliaritySurvey(props: HasuraFamiliaritySurveyProps) {
  const { data, onOptionClick } = props;

  return (
    <>
      <div className="mt-lg">
        <div className="font-bold text-gray-600 mb-xs">{data.question}</div>
        <div className="flex justify-center">
          <IconCardGroup
            items={data.options}
            disabled={false}
            onChange={onOptionClick}
          />
        </div>
      </div>
      {/* Remove skipping survey button, this change is experimental according to analytics data, so only commenting the code */}
      {/* <div className="cursor-pointer text-secondary text-sm hover:text-secondary-dark">
      <Analytics name="hasura-familiarity-survey-skip-button">
        <div
          onClick={onSkip}
        >
          Skip
        </div>
        </Analytics?
      </div> */}
    </>
  );
}
