import { IconCardGroupItem } from '@/new-components/IconCardGroup';
import { SurveysResponseData } from '../../types';
import {
  familiaritySurveyOptionCode,
  FamiliaritySurveyOptionCode,
  getFamiliaritySurveyOptionDetails,
} from './familiaritySurveyOptionDetails';

export const mapOptionLabelToDetails = (
  data: SurveysResponseData['data']['survey'][0]['survey_questions'][0]
): IconCardGroupItem<string>[] => {
  const unroderedOptionArray: {
    option: FamiliaritySurveyOptionCode;
    id: string;
  }[] = [];
  data.survey_question_options.forEach(optionData => {
    const optionLabel = optionData.option.toLowerCase();

    if (
      optionLabel !== 'new user' &&
      optionLabel !== 'past user' &&
      optionLabel !== 'recurring user' &&
      optionLabel !== 'active user'
    ) {
      console.error(
        `Option ${optionLabel} found with no associated handler, did you forget to handle an option?`
      );
      return;
    }

    unroderedOptionArray.push({
      option: optionLabel,
      id: optionData.id,
    });
  });

  // enforce order of options to show in the UI
  const surveyOptionDetails: IconCardGroupItem<string>[] = [];
  familiaritySurveyOptionCode.forEach(optionCode => {
    const optionData = unroderedOptionArray.find(
      optionObj => optionObj.option === optionCode
    );
    if (optionData) {
      const optionValues = getFamiliaritySurveyOptionDetails(
        optionData.id,
        optionCode
      );
      surveyOptionDetails.push(optionValues);
    }
  });

  return surveyOptionDetails;
};
