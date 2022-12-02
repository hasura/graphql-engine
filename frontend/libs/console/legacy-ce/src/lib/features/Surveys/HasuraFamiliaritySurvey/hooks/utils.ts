import { IconCardGroupItem } from '../components/IconCardGroup';
import { SurveysResponseData } from '../../types';
import {
  familiaritySurveyOptionCode,
  FamiliaritySurveyOptionCode,
  getFamiliaritySurveyOptionDetails,
} from './familiaritySurveyOptionDetails';

type UnroderedOptionArray = {
  option: FamiliaritySurveyOptionCode;
  id: string;
}[];

/**
 * Manually enforces the order of options as present in `familiaritySurveyOptionCode`
 * array.
 */
// Not removing this function. Although it is not being used currenty due to dynamic product requirements,
// but can again be used in future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const manuallyOrderOptions = (unroderedOptionArray: UnroderedOptionArray) => {
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

/**
 * Randomly shuffles order of options of survey.
 * The method comes from this algorithm: https://stackoverflow.com/a/46545530/7088648
 */
const randomlyOrderOptions = (unroderedOptionArray: UnroderedOptionArray) => {
  const shuffledOptionArray = unroderedOptionArray
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value);

  const surveyOptionDetails: IconCardGroupItem<string>[] =
    shuffledOptionArray.map(option =>
      getFamiliaritySurveyOptionDetails(option.id, option.option)
    );

  return surveyOptionDetails;
};

export const mapOptionLabelToDetails = (
  data?: SurveysResponseData['data']['survey'][0]['survey_questions'][0]
): IconCardGroupItem<string>[] => {
  if (!data) return [];

  // Array containing order of options as it comes from backend
  const unroderedOptionArray: UnroderedOptionArray = [];
  data?.survey_question_options?.forEach(optionData => {
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

  // return manuallyOrderOptions(unroderedOptionArray);
  return randomlyOrderOptions(unroderedOptionArray);
};
