import React from 'react';
import AceEditor from 'react-ace';

import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  addHeader,
  removeHeader,
  setHeaderKey,
  setHeaderType,
  setHeaderValue,
} from './Actions';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import { Icon, ToolTip, Heading, Text } from '../../../UIKit/atoms';

const HeadersEditor = props => {
  const { dispatch, headers, modifyTrigger, styles, save } = props;

  const addExtraHeader = () => {
    const lastHeader = modifyTrigger.headers[modifyTrigger.headers.length - 1];
    if (lastHeader.key && lastHeader.value && lastHeader.type) {
      dispatch(addHeader());
    }
  };

  const setValues = () => {
    headers.forEach((h, i) => {
      const { name, value, value_from_env } = h;
      dispatch(setHeaderKey(name, i));
      dispatch(setHeaderType(value_from_env ? 'env' : 'static', i));
      dispatch(setHeaderValue(value || value_from_env, i));
      if (!modifyTrigger[i + 1]) {
        addExtraHeader();
      }
    });
  };

  const handleSelectionChange = (e, i) => {
    dispatch(setHeaderType(e.target.getAttribute('value'), i));
    dispatch(setHeaderValue('', i));
  };

  const sanitiseHeaders = rawHeaders => {
    return rawHeaders.map(h => {
      return {
        name: h.name,
        value: h.value,
        value_from_env: h.value_from_env,
      };
    });
  };

  const collapsed = () => (
    <div>
      {headers.length > 0 ? (
        <div className={styles.modifyHeaders}>
          <AceEditor
            mode="json"
            theme="github"
            name="headers"
            value={JSON.stringify(sanitiseHeaders(headers), null, 2)}
            minLines={4}
            maxLines={100}
            width="100%"
            showPrintMargin={false}
            showGutter={false}
            readOnly
          />
        </div>
      ) : (
        <div className={styles.modifyProperty}>
          <Text fontSize="button">No headers</Text>
        </div>
      )}
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyOpsPadLeft}>
      {modifyTrigger.headers.map((h, i) => {
        return (
          <div className={styles.modifyHeadersCollapsedContent} key={i}>
            <input
              type="text"
              className={`${styles.input} form-control ${styles.add_mar_right} ${styles.modifyHeadersTextbox}`}
              value={h.key}
              onChange={e => {
                dispatch(setHeaderKey(e.target.value, i));
              }}
              placeholder="key"
            />
            <div className={styles.dropDownGroup}>
              <DropdownButton
                dropdownOptions={[
                  { display_text: 'Value', value: 'static' },
                  { display_text: 'From env var', value: 'env' },
                ]}
                title={h.type === 'env' ? 'From env var' : 'Value'}
                dataKey={h.type === 'env' ? 'env' : 'static'}
                onButtonChange={e => handleSelectionChange(e, i)}
                onInputChange={e => {
                  dispatch(setHeaderValue(e.target.value, i));
                  addExtraHeader();
                }}
                required
                bsClass={styles.dropdown_button}
                inputVal={h.value}
                id={`header-value-${i}`}
                inputPlaceHolder={
                  h.type === 'env' ? 'HEADER_FROM_ENV' : 'value'
                }
                testId={`header-value-${i}`}
              />
            </div>
            <Icon
              type="close"
              className={styles.removeHeader}
              ml="10px"
              pointer
              onClick={() => {
                dispatch(removeHeader(i));
              }}
            />
          </div>
        );
      })}
    </div>
  );

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <Heading as="h4" fontSize="15px" mb="20px">
          Headers{' '}
          <ToolTip
            message={
              'Edit headers to be sent along with the event to your webhook'
            }
            ml="sm"
          />
        </Heading>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          expandCallback={setValues}
          ongoingRequest={modifyTrigger.ongoingRequest}
          property="headers"
          service="modify-trigger"
          saveFunc={save}
          styles={styles}
        />
      </div>
    </div>
  );
};

export default HeadersEditor;
