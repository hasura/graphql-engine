import React, { useState } from 'react';
import styles from '../Common.scss';

interface HeadingProps {
  editable: boolean, 
  currentValue: string, 
  save: (val: string) => void, 
  loading: boolean, 
  property: string
}

const Heading: React.FC<HeadingProps> = ({ 
  editable, 
  currentValue, 
  save, 
  loading, 
  property 
}) => {
  const [text, setText]= useState(currentValue)
  const [isEditting, setIsEditting] = useState(false)

  const handleTextChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setText(e.target.value);
  };

  const toggleEditting = () => setIsEditting(!isEditting);

  const handleSave = () => {
    if (loading) {
      return;
    }
    save(text);
  };
  
  const handleKeyPress = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (isEditting && e.key === 'Enter') {
      handleSave();
    }
  };

  if (!editable) {
    return <h2 className={styles.heading_text}>{currentValue}</h2>;
  }

  if (!save) {
    console.warn('In EditableHeading, please provide a prop save');
  }

  if (!isEditting) {
    return (
      <div className={styles.editable_heading_text}>
        <h2>{currentValue}</h2>
        <div
          onClick={toggleEditting}
          className={styles.editable_heading_action}
          data-test={`heading-edit-${property}`}
        >
          <i className="fa fa-edit" />
        </div>
      </div>
    );
  }

  return (
    <div className={styles.editable_heading_textbox}>
      <input
        onChange={handleTextChange}
        className={`${styles.add_pad_left} form-control`}
        type="text"
        onKeyPress={handleKeyPress}
        value={text}
        data-test={`heading-edit-${property}-input`}
      />
      <div className={styles.editable_heading_action}>
        <div
          className={styles.editable_heading_action_item}
          onClick={handleSave}
          data-test={`heading-edit-${property}-save`}
        >
          {loading ? 'Saving...' : 'Saved'}
        </div>
        <div
          className={styles.editable_heading_action_item}
          onClick={toggleEditting}
          data-test={`heading-edit-${property}-cancel`}
        >
          Cancel
        </div>
      </div>
    </div>
  );
}

export default Heading;
