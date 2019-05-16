import React, { useState } from 'react';
import AceEditor from 'react-ace';

const styles = require('../Common.scss');

const NORMALKEY = 'normal';
const JSONKEY = 'json';

const parseJSONData = data => {
  try {
    if (typeof data === 'object') {
      return JSON.stringify(data, null, 4);
    }
    return JSON.stringify(JSON.parse(data), null, 4);
  } catch (e) {
    return data;
  }
};

const createInitialState = data => {
  const initialState = {
    showEditorType: NORMALKEY,
    data: parseJSONData(data),
  };
  return initialState;
};

const JsonInput = props => {
  const { standardProps, placeholderProp } = props;
  const { defaultValue } = standardProps;
  const allProps = { ...standardProps };
  delete allProps.defaultValue;
  const [state, updateState] = useState(createInitialState(defaultValue));
  const { showEditorType, data } = state;
  const updateData = newData => {
    return {
      ...state,
      data: newData,
    };
  };
  const toggleEditorType = () => {
    if (state.showEditorType === JSONKEY) {
      return {
        ...state,
        showEditorType: NORMALKEY,
      };
    }
    return {
      ...state,
      data: parseJSONData(state.data),
      showEditorType: JSONKEY,
    };
  };
  const handleKeyUpEvent = e => {
    if (e.ctrlKey && e.which === 32) {
      updateState(toggleEditorType());
    }
  };
  const renderHtml =
    showEditorType === JSONKEY ? (
      <AceEditor
        key="ace_json_editor"
        {...allProps}
        mode="json"
        theme="github"
        name="jsontoggler"
        minLines={10}
        maxLines={100}
        width="100%"
        value={data}
        showPrintMargin={false}
        showGutter={false}
        onChange={val => updateState(updateData(val))}
        onFocus={e => console.log(e)}
      />
    ) : (
      <input
        key="input_json_editor"
        {...allProps}
        placeholder={`${placeholderProp} (Ctrl + Space to toggle)`}
        value={data}
        onChange={e => updateState(updateData(e.target.value))}
        onKeyUp={handleKeyUpEvent}
      />
    );
  return (
    <span>
      <label>{renderHtml}</label>
      <i
        key="icon_json_editor"
        className={
          (state.showEditorType === JSONKEY
            ? 'fa fa-compress '
            : 'fa fa-expand ') + styles.jsonButtonAlign
        }
        onClick={() => updateState(toggleEditorType())}
      />
    </span>
  );
};

/*
class Jsontoggler extends Component {
  constructor(props) {
    super(props);

    this.state = {
      input_json: 0,
    };

    this.handleClick = this.handleClick.bind(this);
    this.parseString = this.parseString.bind(this);
  }

  handleClick(e) {
    e.preventDefault();
    const stateValue = this.state.input_json;
    if (stateValue === 0) {
      this.setState({ input_json: 1 });
    } else {
      this.setState({ input_json: 0 });
    }
  }

  parseString() {
    const { defaultValue } = this.props.standardProps;
    try {
      if (typeof defaultValue === 'object') {
        return JSON.stringify(defaultValue, null, 4);
      }
      return JSON.stringify(JSON.parse(defaultValue), null, 4);
    } catch (e) {
      return defaultValue;
    }
  }

  getValueFromRelatedInput() {
    const { refNode } = this.props;
    console.log('RefNode');
    console.log(refNode);
    if (refNode && 'valueNode' in refNode && refNode.valueNode) {
      if ('props' in refNode.valueNode) {
        return refNode.valueNode.refEditor.innerText;
      }
      return refNode.valueNode.value;
    }
    return '';
  }

  render() {
    const initialVal = this.getValueFromRelatedInput() || this.parseString();
    console.log('initialVal');
    console.log(initialVal);
    return (
      <span>
        <label>
          {this.state.input_json === 1 ? (
            <AceEditor
              {...this.props.standardProps}
              mode="json"
              theme="github"
              name="jsontoggler"
              minLines={10}
              maxLines={100}
              width="100%"
              defaultValue={initialVal}
              showPrintMargin={false}
              showGutter={false}
            />
          ) : (
            <input
              {...this.props.standardProps}
              placeholder={this.props.placeholderProp}
              defaultValue={initialVal}
            />
          )}
        </label>
        <i
          className={
            (this.state.input_json === 1
              ? 'fa fa-compress '
              : 'fa fa-expand ') + styles.jsonButtonAlign
          }
          onClick={this.handleClick}
        />
      </span>
    );
  }
} */

export default JsonInput;
