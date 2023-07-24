import { useFormContext } from 'react-hook-form';
import { CommentInput } from './CommentInput';

export function CommentSection() {
  const { watch, setValue } = useFormContext();

  const comment = watch('comment');
  return (
    <CommentInput
      comment={comment}
      setComment={newComment => {
        setValue('comment', newComment);
      }}
    />
  );
}
