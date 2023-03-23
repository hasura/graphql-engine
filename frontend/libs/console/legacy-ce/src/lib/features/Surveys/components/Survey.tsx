import React, { useMemo } from 'react';
import { AllowedSurveyThemes, SurveyData } from '../types';
import { orderArrayItems } from '../utils';
import { SurveyResponseV2 } from '../../ControlPlane';
import { FamiliaritySurveyTheme } from './FamiliaritySurveyTheme';

type Props = {
  theme: AllowedSurveyThemes;
  onSubmit: (arg: SurveyResponseV2[]) => void;
  data?: SurveyData;
  show?: boolean;
  onSkip?: () => void;
  onSuccessCb?: () => void;
  onRejectCb?: () => void;
};

/**
 * Survey component which acts as a top level wrapper for different themes.
 * This component have some common logic used across all surveys, like ordering
 * the questions and getting submit/cancel text. This does not manage the state of
 * specific survey theme, state management is done by the themes itself. As different themes
 * can have different state management logic (from simple `useState` to using react-hook-form for
 * more complex surveys). State management for themes can be unified in the common component
 * in future if need arises.
 */
export const Survey: React.FC<Props> = props => {
  const { onSubmit, onSkip, data, onSuccessCb, onRejectCb, theme } = props;

  // position the question order before showing in survey
  const orderedSurveyQuestionData = useMemo(() => {
    if (!data) return [];
    return orderArrayItems(data.survey_questions);
  }, [data?.survey_questions]);

  if (!data) return null;

  if (!orderedSurveyQuestionData.length) return null;

  const handleSkip = () => {
    if (onSkip) onSkip();
    if (onRejectCb) {
      onRejectCb();
    }
  };

  const handleSubmit = (data: SurveyResponseV2[]) => {
    onSubmit(data);
    if (onSuccessCb) {
      onSuccessCb();
    }
  };

  // Utility to get submit/cancel text.
  // const { submitText, cancelText } = getButtonText(show ?? false, data);

  if (theme === AllowedSurveyThemes.familiaritySurveyTheme) {
    return (
      <FamiliaritySurveyTheme
        orderedSurveyQuestionData={orderedSurveyQuestionData}
        handleSubmit={handleSubmit}
        handleSkip={handleSkip}
      />
    );
  }

  return null;
};
