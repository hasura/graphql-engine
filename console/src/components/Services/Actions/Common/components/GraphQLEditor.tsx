import React from 'react';
import { parse as sdlParser } from 'graphql/language/parser';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import CrossIcon from '../../../../Common/Icons/Cross';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { Nullable } from '../../../../Common/utils/tsUtils';

type GraphQLEditorProps = {
    value: string;
    onChange: (value: Nullable<Record<string, any>>, error: Nullable<Error>, timer: Nullable<() => void>, ast: Nullable<Record<string, any>>) => void;
    className?: string;
    placeholder: string;
    error: Error;
    timer: number;
    readOnlyMode: boolean;
    label: string;
    tooltip: string;
    height?: string;
};

const GraphQLEditor: React.FC<GraphQLEditorProps> = ({
    value,
    onChange,
    className,
    placeholder = "",
    error,
    timer,
    readOnlyMode,
    label,
    tooltip,
    height,
}) => {
    const onChangeWithError = (value: any) => {
        if (timer) {
            clearTimeout(timer);
        }

        const parseDebounceTimer = () => {
            setTimeout(() => {
                let error = null;
                let ast = null;
                try {
                    ast = sdlParser(value);
                } catch (err) {
                    error = err;
                }
                onChange(null, error, null, ast);
            }, 1000);

            onChange(value, null, parseDebounceTimer, null);
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
                    height={height || "200px"}
                    mode="graphqlschema"
                    width="600px"
                    readOnly={readOnlyMode}
                />
            </div>
        </div>
    );
};

export const actionDefinitionInfo = {
    label: "Action definition",
    tooltip: "Define the action as a query or a mutation using GraphQL SDL. You can use the custom types already defined by you or define new types in the new types definition editor below."
};

export const typeDefinitionInfo = {
    label: "New types definition",
    tooltip: "You can define new GraphQL types that you can use in the action definition above",
};

export default GraphQLEditor;
