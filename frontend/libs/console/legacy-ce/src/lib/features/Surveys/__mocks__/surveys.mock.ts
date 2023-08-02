import { Survey_V2_Question_Kind_Enum } from '../../ControlPlane';
import { SurveysResponseData } from '../types';

export const mockFetchAllSurveysData: SurveysResponseData = {
  data: {
    survey_v2: [
      {
        survey_name: 'Hasura Cloud Project Downgrade Survey',
        survey_title: "😟 We're sorry to see you downgrade!",
        survey_description: 'Could you give us some quick feedback to improve?',
        survey_questions: [
          {
            id: '4d668d32-013e-48af-9a21-a754cb2d783d',
            position: 1,
            question: 'Why are you looking to downgrade this project?',
            kind: 'radio' as Survey_V2_Question_Kind_Enum,
            is_mandatory: true,
            survey_question_options: [
              {
                id: 'a61a7bcf-dfac-4887-9607-a6bef68ff474',
                position: 1,
                option: 'Sticking with Hasura, but cheaper to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '7f45c232-ea64-4780-b8f2-5d2d811d7707',
                position: 1,
                option:
                  'Sticking with Hasura, but more stable to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: 'e2a22248-aa46-4eaa-89ec-1a4a4e10e87d',
                position: 1,
                option: 'Sticking with Hasura, but faster to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '5ba72030-ce9f-49d7-857f-70d0e47db981',
                position: 1,
                option:
                  "Hasura is great. I'm just not using this project anymore",
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '5f474d42-55b3-4286-9366-56d264e8ed0d',
                position: 1,
                option:
                  "Hasura is lacking features, I'm moving to another vendor",
                template_config: null,
                additional_info_config: {
                  info_description:
                    'Provide further details regarding the features you want',
                  is_mandatory: true,
                },
              },
              {
                id: '2d6ce8bf-9dca-46b2-9d1b-db71be07cf46',
                position: 1,
                option:
                  "Hasura is lacking features. I'm going to build GraphQL Api using a library",
                template_config: null,
                additional_info_config: {
                  info_description:
                    'Provide further details regarding the features you want',
                  is_mandatory: true,
                },
              },
              {
                id: 'f297693c-53ee-4b5d-b4d1-22343699066c',
                position: 1,
                option: 'I created this project by mistake',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
            ],
          },
        ],
        template_config:
          '{ "survey_submit_text": "Submit and confirm downgrade", "survey_cancel_text": "Cancel project downgrade" }',
        survey_responses: [],
      },
      {
        survey_name: 'Hasura Cloud Project Delete Survey',
        survey_title: "😟 We're sorry to see you leave!",
        survey_description: 'Could you give us some quick feedback to improve?',
        survey_questions: [
          {
            id: 'b233b6c5-2caf-41af-bb78-1ea59df40d0b',
            position: 1,
            question: 'Why are you looking to delete this project?',
            kind: 'radio' as Survey_V2_Question_Kind_Enum,
            is_mandatory: true,
            survey_question_options: [
              {
                id: 'd2d7a79b-5a6a-4d55-af76-0fceff004303',
                position: 1,
                option: 'Sticking with Hasura, but cheaper to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '04f5ecce-dcd9-4a41-9630-31c25dceca48',
                position: 1,
                option:
                  'Sticking with Hasura, but more stable to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: 'bdb03617-2a5c-4be2-afde-b74199a11b25',
                position: 1,
                option: 'Sticking with Hasura, but faster to run it myself',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '5a92977f-0c8c-4968-88e2-d836a291d53d',
                position: 1,
                option:
                  "Hasura is great. I'm just not using this project anymore",
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
              {
                id: '4be11e55-0855-4de5-8e4d-dac9d8dbb83b',
                position: 1,
                option:
                  "Hasura is lacking features, I'm moving to another vendor",
                template_config: null,
                additional_info_config: {
                  info_description:
                    'Provide further details regarding the features you want',
                  is_mandatory: true,
                },
              },
              {
                id: '8db2d891-07a9-498e-84f7-675e2bec6b66',
                position: 1,
                option:
                  "Hasura is lacking features. I'm going to build GraphQL Api using a library",
                template_config: null,
                additional_info_config: {
                  info_description:
                    'Provide further details regarding the features you want',
                  is_mandatory: true,
                },
              },
              {
                id: 'fba9de29-67e3-48ad-ba5a-e10a090d1f3f',
                position: 1,
                option: 'I created this project by mistake',
                template_config: null,
                additional_info_config: {
                  info_description:
                    "Any other information you'd like to include for our team",
                  is_mandatory: false,
                },
              },
            ],
          },
        ],
        template_config:
          '{ "survey_submit_text": "Submit and confirm delete", "survey_cancel_text": "Cancel project delete" }',
        survey_responses: [],
      },
      {
        survey_name: 'Hasura familiarity survey',
        survey_title: null,
        survey_description: null,
        survey_questions: [
          {
            id: '15c5ef6c-3a98-4833-851a-40aa66940adc',
            position: 1,
            question: 'How familiar are you with Hasura?',
            kind: 'radio' as Survey_V2_Question_Kind_Enum,
            is_mandatory: true,
            survey_question_options: [
              {
                id: '180c1afb-c313-4a2d-b32a-5d89af829fad',
                position: 1,
                option: "I'm completely new to Hasura",
                template_config:
                  '{ "react_icons_fa_component_name": "FaHeart" }',
                additional_info_config: null,
              },
              {
                id: '5d9fc927-37a2-4ebe-99c9-63089ee45cfa',
                position: 1,
                option:
                  "I've used Hasura before but not actively developing right now",
                template_config:
                  '{ "react_icons_fa_component_name": "FaBookmark" }',
                additional_info_config: null,
              },
              {
                id: '4f5de190-6ec1-4a59-9074-f25b8023798b',
                position: 1,
                option: "I'm already using Hasura (CE/Cloud) weekly/monthly",
                template_config:
                  '{ "react_icons_fa_component_name": "FaUser" }',
                additional_info_config: null,
              },
              {
                id: '508234fc-c86c-459f-af1b-7c7281805af5',
                position: 1,
                option: "I'm actively developing with Hasura (CE/Cloud) daily",
                template_config:
                  '{ "react_icons_fa_component_name": "FaStar" }',
                additional_info_config: null,
              },
            ],
          },
        ],
        template_config: null,
        survey_responses: [
          {
            survey_response_answers: [
              {
                survey_question_id: '15c5ef6c-3a98-4833-851a-40aa66940adc',
                survey_response_answer_options: [
                  {
                    answer: null,
                    additional_info: null,
                    option_id: '5d9fc927-37a2-4ebe-99c9-63089ee45cfa',
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  },
};
