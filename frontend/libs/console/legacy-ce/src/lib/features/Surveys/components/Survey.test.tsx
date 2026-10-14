import { render, screen } from '@testing-library/react';
import { Survey } from './Survey';
import { AllowedSurveyThemes, SurveyData } from '../types';

// Render the ordered question ids so we can assert the memoized ordering output.
vi.mock('./FamiliaritySurveyTheme', () => ({
  FamiliaritySurveyTheme: ({
    orderedSurveyQuestionData,
  }: {
    orderedSurveyQuestionData: Array<{ id: string }>;
  }) => (
    <div data-testid="familiarity">
      {orderedSurveyQuestionData.map((q) => q.id).join(',')}
    </div>
  ),
}));

const makeData = (
  questions: Array<{ id: string; position: number }>,
): SurveyData => ({ survey_questions: questions }) as unknown as SurveyData;

describe('Survey', () => {
  it('renders nothing when there is no data', () => {
    const { container } = render(
      <Survey
        theme={AllowedSurveyThemes.familiaritySurveyTheme}
        onSubmit={vi.fn()}
      />,
    );
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing when there are no survey questions', () => {
    const { container } = render(
      <Survey
        theme={AllowedSurveyThemes.familiaritySurveyTheme}
        onSubmit={vi.fn()}
        data={makeData([])}
      />,
    );
    expect(container).toBeEmptyDOMElement();
  });

  it('renders the familiarity theme with questions ordered by position', () => {
    render(
      <Survey
        theme={AllowedSurveyThemes.familiaritySurveyTheme}
        onSubmit={vi.fn()}
        data={makeData([
          { id: 'b', position: 2 },
          { id: 'a', position: 1 },
        ])}
      />,
    );
    expect(screen.getByTestId('familiarity')).toHaveTextContent('a,b');
  });

  it('recomputes the ordered questions when survey_questions change', () => {
    const { rerender } = render(
      <Survey
        theme={AllowedSurveyThemes.familiaritySurveyTheme}
        onSubmit={vi.fn()}
        data={makeData([{ id: 'a', position: 1 }])}
      />,
    );
    expect(screen.getByTestId('familiarity')).toHaveTextContent('a');

    rerender(
      <Survey
        theme={AllowedSurveyThemes.familiaritySurveyTheme}
        onSubmit={vi.fn()}
        data={makeData([
          { id: 'x', position: 1 },
          { id: 'y', position: 2 },
        ])}
      />,
    );
    expect(screen.getByTestId('familiarity')).toHaveTextContent('x,y');
  });
});
