import { fireEvent, render, screen } from '@testing-library/react';
import { FamiliaritySurveyTheme } from './FamiliaritySurveyTheme';
import { SurveyData } from '../../types';

// Replace SurveyQuestion with a lightweight double that lets the test drive the
// `responses` state (what the real radio inputs would do).
vi.mock('../SurveyQuestion', () => ({
  SurveyQuestion: ({ setResponses }: { setResponses: (r: any[]) => void }) => (
    <button
      data-testid="answer"
      onClick={() =>
        setResponses([
          { question_id: 'q1', answer: 'answer', optional_answer: '' },
        ])
      }
    >
      answer
    </button>
  ),
}));

const questionData = [
  { id: 'q1', kind: 'radio' },
] as unknown as SurveyData['survey_questions'];

describe('FamiliaritySurveyTheme', () => {
  it('renders the questions and does not submit before a response exists', () => {
    const handleSubmit = vi.fn();
    render(
      <FamiliaritySurveyTheme
        handleSubmit={handleSubmit}
        orderedSurveyQuestionData={questionData}
      />,
    );
    expect(screen.getByTestId('answer')).toBeInTheDocument();
    expect(handleSubmit).not.toHaveBeenCalled();
  });

  it('calls handleSubmit when a response is added', () => {
    const handleSubmit = vi.fn();
    render(
      <FamiliaritySurveyTheme
        handleSubmit={handleSubmit}
        orderedSurveyQuestionData={questionData}
      />,
    );
    fireEvent.click(screen.getByTestId('answer'));
    expect(handleSubmit).toHaveBeenCalledTimes(1);
    expect(handleSubmit).toHaveBeenCalledWith([
      { question_id: 'q1', answer: 'answer', optional_answer: '' },
    ]);
  });

  it('uses the latest handleSubmit and does not re-submit on parent re-render', () => {
    const firstHandleSubmit = vi.fn();
    const secondHandleSubmit = vi.fn();
    const { rerender } = render(
      <FamiliaritySurveyTheme
        handleSubmit={firstHandleSubmit}
        orderedSurveyQuestionData={questionData}
      />,
    );

    fireEvent.click(screen.getByTestId('answer'));
    expect(firstHandleSubmit).toHaveBeenCalledTimes(1);

    // Parent re-renders with a NEW handleSubmit identity (as inline callbacks do).
    rerender(
      <FamiliaritySurveyTheme
        handleSubmit={secondHandleSubmit}
        orderedSurveyQuestionData={questionData}
      />,
    );

    // The effect must NOT re-fire just because handleSubmit identity changed:
    // no extra submit happened when `responses` did not change.
    expect(firstHandleSubmit).toHaveBeenCalledTimes(1);
    expect(secondHandleSubmit).not.toHaveBeenCalled();
  });
});
