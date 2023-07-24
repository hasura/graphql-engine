import clsx from 'clsx';
import TextareaAutosize from 'react-autosize-textarea';
import { FaRegComment } from 'react-icons/fa';

export function CommentInput({
  comment,
  setComment,
}: {
  comment: string | undefined;
  setComment: (comment: string) => void;
}) {
  return (
    <div className="pb-4">
      <p className="m-0 mb-1 text-gray-600">
        <strong>Comments</strong>
      </p>
      <div className="relative">
        <TextareaAutosize
          style={{ minWidth: '24rem', outline: 'none' }}
          rows={1}
          className={clsx(
            'border border-gray-300 border-l-4 border-l-secondary p-sm peer',
            'focus:bg-white rounded focus:[box-shadow:none] focus:border-secondary',
            'placeholder-shown:italic pl-9',
            comment ? 'bg-secondary-light' : 'bg-white'
          )}
          name="comments"
          value={comment}
          onChange={e => {
            setComment(e.currentTarget.value);
          }}
          placeholder="Add a comment explaining the permissions"
        />
        <FaRegComment
          className={clsx(
            'absolute opacity-50 left-0 top-1/2 -translate-y-1/2 -mt-[4px] ml-4'
          )}
        />
      </div>
    </div>
  );
}
