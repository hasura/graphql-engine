import React from 'react';
import { GraphQLError } from 'graphql';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import { Nullable } from '../../../../Common/utils/tsUtils';
import GraphQLEditor from '../../../../Common/GraphQLEditor/GraphQLEditor';

type GraphQLEditorProps = {
  value: string;
  onChange: (
    value: Nullable<string>,
    error: Nullable<Error>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => void;
  className?: string;
  placeholder: string;
  error: GraphQLError;
  timer: number;
  readOnlyMode: boolean;
  label: string;
  tooltip: string;
  height?: string;
  allowEmpty?: boolean;
};

const GraphQLEditorWrapper: React.FC<GraphQLEditorProps> = ({
  value,
  onChange,
  className,
  placeholder = '',
  error,
  timer,
  readOnlyMode,
  label,
  tooltip,
  height,
  allowEmpty = false,
}) => {
  return (
    <div className={`${className || ''}`}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {label}
        <Tooltip
          id="action-name"
          text={tooltip}
          className={styles.add_mar_left_mid}
        />
      </h2>
      <div className={styles.sdlEditorContainer}>
        <GraphQLEditor
          value={value}
          onChange={onChange}
          placeholder={placeholder}
          error={error}
          timer={timer}
          readOnlyMode={readOnlyMode}
          height={height || '200px'}
          allowEmpty={allowEmpty}
        />
      </div>
    </div>
  );
};

export default GraphQLEditorWrapper;
