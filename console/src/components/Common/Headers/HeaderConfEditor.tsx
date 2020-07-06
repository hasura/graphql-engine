import React from 'react';
import styles from './Headers.scss';
import Tooltip from '../../Services/Actions/Common/components/Tooltip';
import Headers, { Header } from './Headers';

export interface HeaderConfEditorProps {
  editorTitle: string;
  module: string;
  editorTitleToolTip: string;
  className: string;
  additionalHeadersToolTip: string;
  forwardClientHeaders: boolean;
  disabled: boolean;
  headers: Header[];
  toggleForwardClientHeaders: () => void;
  setHeaders: (headers: Header[]) => void;
}
const HeaderConfEditor: React.FC<HeaderConfEditorProps> = ({
  editorTitle = 'Headers',
  module,
  editorTitleToolTip,
  className,
  forwardClientHeaders,
  toggleForwardClientHeaders,
  headers,
  setHeaders,
  additionalHeadersToolTip,
  disabled = false,
}) => {
  return (
    <div className={className || ''}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorTitle}
        {editorTitleToolTip && (
          <Tooltip
            id="action-name"
            text={editorTitleToolTip}
            className={styles.add_mar_left_mid}
          />
        )}
      </h2>
      <div className={`${styles.add_mar_bottom_mid}`}>
        <label className={`${styles.add_mar_right} ${styles.cursorPointer}`}>
          <input
            type="checkbox"
            checked={forwardClientHeaders}
            onChange={toggleForwardClientHeaders}
            className={`${styles.display_inline} ${styles.add_mar_right}`}
            disabled={disabled}
          />
          Forward all headers from client
        </label>
      </div>
      <div className={`${styles.font_normal} ${styles.add_pad_bottom}`}>
        Additional headers:
        <Tooltip
          id="action-name"
          text={additionalHeadersToolTip}
          className={styles.add_mar_left_mid}
        />
      </div>

      <Headers
        headers={headers}
        setHeaders={setHeaders}
        disabled={disabled}
        module={module}
      />
    </div>
  );
};

export default HeaderConfEditor;
