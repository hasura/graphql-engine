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

  React.useEffect(() => {
    if (responses.length > 0) {
      handleSubmit(responses);
    }
  }, [responses]);

  return (
    <div className="mt-lg">
      {orderedSurveyQuestionData.map(questionData => (
        <SurveyQuestion
          questionData={questionData}
          responses={responses}
          setResponses={setResponses}
        />
      ))}
    </div>
  );
}
