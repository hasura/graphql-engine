import { Button } from '../../../../new-components/Button';
import React, { useRef } from 'react';
import { FaUpload } from 'react-icons/fa';

interface UploadFileProps {
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

// create a file input with a button triggering it
export const UploadFile = (props: UploadFileProps) => {
  const { onChange } = props;
  const inputRef = useRef<HTMLInputElement>(null);
  const handleClick = React.useCallback(() => {
    if (inputRef.current) {
      inputRef.current.click();
    }
  }, []);
  return (
    <div>
      <input
        style={{ display: 'none' }}
        ref={inputRef}
        type="file"
        onChange={onChange}
        data-testid="file"
      />

      <Button icon={<FaUpload />} iconPosition="end" onClick={handleClick}>
        From File
      </Button>
    </div>
  );
};
