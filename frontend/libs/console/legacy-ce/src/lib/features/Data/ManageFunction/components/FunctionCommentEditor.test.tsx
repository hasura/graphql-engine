import { render, screen } from '@testing-library/react';
import { FunctionCommentEditor } from './FunctionCommentEditor';

// Function comments cannot be edited from the console because
// `<driver>_set_function_customization` has no `comment` field (only the
// function configuration can be updated). The component therefore shows an
// existing comment read-only and renders nothing when there is no comment.
describe('FunctionCommentEditor', () => {
  describe('when the function has a non-empty comment', () => {
    it('shows the comment text read-only under the "Function Comments" heading', () => {
      render(<FunctionCommentEditor comment="existing comment" />);

      expect(screen.getByText('Function Comments')).toBeInTheDocument();
      expect(screen.getByText('existing comment')).toBeInTheDocument();
    });

    it('renders no edit/save controls', () => {
      render(<FunctionCommentEditor comment="existing comment" />);

      // No trigger to open an editor...
      expect(
        screen.queryByRole('button', { name: /edit comment/i }),
      ).not.toBeInTheDocument();
      expect(
        screen.queryByRole('button', { name: /add a comment/i }),
      ).not.toBeInTheDocument();
      // ...no editable field...
      expect(
        screen.queryByLabelText('Function comment'),
      ).not.toBeInTheDocument();
      // ...and no way to persist a change.
      expect(
        screen.queryByRole('button', { name: /save/i }),
      ).not.toBeInTheDocument();
    });
  });

  describe('when the function has no comment', () => {
    it.each([
      ['an empty string', ''],
      ['null', null],
      ['undefined', undefined],
    ])('renders nothing when the comment is %s', (_label, comment) => {
      const { container } = render(<FunctionCommentEditor comment={comment} />);

      expect(container).toBeEmptyDOMElement();
      expect(screen.queryByText('Function Comments')).not.toBeInTheDocument();
    });
  });
});
