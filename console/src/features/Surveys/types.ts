export type SurveysResponseData = {
  data: {
    survey: [
      {
        survey_name: string;
        survey_questions: [
          {
            kind: string;
            question: string;
            id: string;
            survey_question_options: [
              {
                // Note: this value comes from the cloud backend, and is not strongly typed in backend.
                // As the form is dynamic in nature, there could be additions/deletions to survey options in future
                option: string;
                id: string;
              }
            ];
          }
        ];
      }
    ];
    survey_question_answers: [
      {
        survey_question_id: string;
      }
    ];
  };
};

export type QuestionAnswers = {
  surveyName: string;
  responses: [
    {
      answer?: string;
      optionsSelected?: string;
      question_id: string;
    }
  ];
};
