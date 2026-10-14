import React from 'react';
import { SurveyResponseV2 } from '../../../ControlPlane';
import { SurveyQuestion } from '../SurveyQuestion';
import { SurveyData } from '../../types';

type HasuraFamiliaritySurveyProps = {
  handleSubmit: (arg: SurveyResponseV2[]) => void;
  orderedSurveyQuestionData: SurveyData['survey_questions'];
  handleSkip?: () => void;
};

// As the familiarity survey is rendered inside of the onboarding wizard,
// it doesn't contain any wrapper dialog component. As such its a very specific
// use case hence the theme name.
export function FamiliaritySurveyTheme(props: HasuraFamiliaritySurveyProps) {
  const { handleSubmit, orderedSurveyQuestionData } = props;

  const [responses, setResponses] = React.useState<SurveyResponseV2[]>([]);

  // `handleSubmit` is passed inline by the parent and re-created every render, so
  // it must not be an effect dependency (that would re-submit on every parent
  // render). The effect's real trigger is `responses` changing; we read the
  // latest `handleSubmit` from a ref so submission always uses the current one.
  const handleSubmitRef = React.useRef(handleSubmit);
  React.useEffect(() => {
    handleSubmitRef.current = handleSubmit;
  });

  React.useEffect(() => {
    if (responses.length > 0) {
      handleSubmitRef.current(responses);
    }
  }, [responses]);

  return (
    <div className="mt-6">
      {orderedSurveyQuestionData.map((questionData) => (
        <SurveyQuestion
          key={questionData.id}
          questionData={questionData}
          responses={responses}
          setResponses={setResponses}
        />
      ))}
    </div>
  );
}
