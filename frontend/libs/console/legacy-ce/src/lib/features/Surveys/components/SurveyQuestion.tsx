import { SurveyQuestionProps } from '../types';
import { RadioQuestion } from './Common/RadioQuestion/RadioQuestion';

export function SurveyQuestion(props: SurveyQuestionProps) {
  const { questionData, responses, setResponses } = props;

  if (questionData.kind === 'radio') {
    return (
      <RadioQuestion
        questionData={questionData}
        responses={responses}
        setResponses={setResponses}
      />
    );
  }

  return null;
}
