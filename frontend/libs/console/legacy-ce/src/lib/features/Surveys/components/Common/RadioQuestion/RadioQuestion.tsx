import { useMemo } from 'react';
import { FaOptionIcon } from './FaOptionIcon';
import { IconCardGroup, IconCardGroupItem } from './IconCardGroup';
import { SurveyQuestionProps } from '../../../types';
import { orderArrayItems } from '../../../utils';
import { Flex } from '@radix-ui/themes';

export function RadioQuestion(props: SurveyQuestionProps) {
  const { questionData, responses, setResponses } = props;

  const onChange = (optionId: string) => {
    setResponses([
      ...responses,
      {
        optionSelected: optionId,
        questionId: questionData.id,
      },
    ]);
  };

  // order the option position before showing
  const surveyOptions: IconCardGroupItem<string>[] = useMemo(() => {
    const orderedOptions = orderArrayItems(
      questionData.survey_question_options,
    );

    return (
      orderedOptions.map((val) => {
        let template_config;
        try {
          template_config = JSON.parse(val.template_config);
        } catch (e) {
          console.error(e);
        }
        return {
          value: val.id,
          icon: FaOptionIcon(template_config?.react_icons_fa_component_name),
          title: val.id,
          body: val.option,
        };
      }) ?? []
    );
  }, [questionData.survey_question_options]);

  return (
    <>
      <div className="font-bold text-gray-600 mb-1">
        {questionData.question}
      </div>
      <Flex justify="center">
        <IconCardGroup
          items={surveyOptions}
          disabled={false}
          onChange={onChange}
        />
      </Flex>
    </>
  );
}
