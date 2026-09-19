import { render, screen } from '@testing-library/react';
import { HighlightText } from './HighlightText';

describe('HighlightText', () => {
  it('highlights the part of the text matching the search term', () => {
    render(<HighlightText text="user_address" highlightedText="address" />);

    expect(screen.getByTestId('user_address')).toHaveTextContent(
      /^user_address$/
    );
    expect(screen.getByText('address').tagName).toBe('STRONG');
  });

  it('matches the search term case-insensitively', () => {
    render(<HighlightText text="UserAddress" highlightedText="address" />);

    expect(screen.getByText('Address').tagName).toBe('STRONG');
  });

  it('renders the text as is when there is no search term', () => {
    render(<HighlightText text="user_address" highlightedText="" />);

    expect(screen.getByTestId('user_address')).toHaveTextContent(
      /^user_address$/
    );
    expect(screen.queryByRole('strong')).not.toBeInTheDocument();
  });

  // the search term is whatever the user types in the sidebar search box, so it
  // has to be escaped before it is used to build a RegExp
  it.each(['(', ')', '[', ']', '{', '}', '*', '+', '?', '|', '^', '$', '\\'])(
    'renders the text when the search term contains %s',
    metacharacter => {
      render(
        <HighlightText text="user_address" highlightedText={metacharacter} />
      );

      expect(screen.getByTestId('user_address')).toHaveTextContent(
        /^user_address$/
      );
    }
  );

  it('treats metacharacters in the search term as literal characters', () => {
    render(
      <HighlightText text="orders_(archived)" highlightedText="(archived)" />
    );

    expect(screen.getByTestId('orders_(archived)')).toHaveTextContent(
      /^orders_\(archived\)$/
    );
    expect(screen.getByText('(archived)').tagName).toBe('STRONG');
  });
});
