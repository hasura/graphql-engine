import React from 'react';
import { parse as sdlParse } from 'graphql/language/parser';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import CrossIcon from '../../../../Common/Icons/Cross';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { Nullable } from '../../../../Common/utils/tsUtils';

type GraphQLEditorProps = {
    value: string;
    onChange: (value: any, error: Nullable<Error>, timer: Nullable<() => void>, ast: any) => void;
    className: string;
    placeholder: string;
    error: Error;
    timer: number;
    readOnlyMode: boolean;
    label: string;
    tooltip: string;
};

const GraphQLEditor: React.FC<GraphQLEditorProps> = ({
    value,
    onChange,
    className,
    placeholder,
    error,
    timer,
    readOnlyMode,
    label,
    tooltip
}) => {
    const onChangeWithError = (v: any) => {
        if (timer) {
            clearTimeout(timer);
        }

        const parseDebounceTimer = () => {
            setTimeout(() => {
                let _e = null;
                let ast = null;
                try {
                    ast = sdlParse(v);
                } catch (e) {
                    _e = e;
                }
                onChange(null, _e, null, ast);
            }, 1000);

            onChange(v, null, parseDebounceTimer, null);
        };
    };

    const errorMessage =
        error && (error.message || 'This is not valid GraphQL SDL');

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
                <div
                    className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}
                >
                    {error && (
                        <div className={`${styles.display_flex}  ${styles.errorMessage}`}>
                            <CrossIcon className={styles.add_mar_right_small} />
                            <div>{errorMessage}</div>
                        </div>
                    )}
                </div>
                <AceEditor
                    name="sdl-editor"
                    value={value}
                    onChange={onChangeWithError}
                    placeholder={placeholder}
                    height="200px"
                    mode="graphqlschema"
                    width="600px"
                    readOnly={readOnlyMode}
                />
            </div>
        </div>
    );
};

export default GraphQLEditor;
