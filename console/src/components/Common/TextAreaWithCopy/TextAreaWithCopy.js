/* eslint react/no-danger: 0 */ // Disabling no-danger rule to use dangerouslySetInnerHTML

import React from 'react';

import PropTypes from 'prop-types';

class TextAreaWithCopy extends React.Component {
  copyToClip(id, e) {
    e.preventDefault();
    const { copyText, textLanguage } = this.props;
    let text = '';
    if (this.props.copyText.length > 0) {
      text =
        window.sqlFormatter &&
        textLanguage &&
        textLanguage.toLowerCase() === 'sql'
          ? window.sqlFormatter.format(copyText, { language: textLanguage })
          : copyText;
    }
    const { containerId } = this.props;

    const textArea = document.createElement('textarea');
    const appendLoc = containerId
      ? document.getElementById(containerId)
      : document.body;
    textArea.value = text;
    appendLoc.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      const successful = document.execCommand('copy');
      // const msg = successful ? 'successful' : 'unsuccessful';
      const tooltip = document.getElementById(id);
      if (!successful) {
        tooltip.innerHTML = 'Error copying';
        throw new Error('Copy was unsuccessful');
      } else {
        tooltip.innerHTML = 'Copied';
      }
    } catch (err) {
      alert('Oops, unable to copy - ' + err);
    }
    appendLoc.removeChild(textArea);
  }

  resetCopy(id) {
    const tooltip = document.getElementById(id);
    tooltip.innerHTML = 'Copy';
  }

  render() {
    const style = require('./TextAreaWithCopy.scss');
    const { copyText, toolTipClass, id, containerId } = this.props;
    const renderSimpleValue = () => {
      return (
        <pre className={style.schemaPreWrapper}>
          <code className={style.formattedCode}>{copyText}</code>
        </pre>
      );
    };

    const renderSQLValue = () => {
      return (
        <pre>
          <code
            className={style.formattedCode}
            dangerouslySetInnerHTML={{
              __html: window.hljs.highlight(
                'sql',
                window.sqlFormatter.format(copyText, {
                  language: this.props.textLanguage,
                })
              ).value,
            }}
          />
        </pre>
      );
    };
    const renderJSONValue = () => {
      return (
        <pre>
          <code
            className={style.formattedCode}
            dangerouslySetInnerHTML={{
              __html: window.hljs.highlight(
                'json',
                JSON.stringify(JSON.parse(copyText), null, 4)
              ).value,
            }}
          />
        </pre>
      );
    };
    const typeRenderer = {
      sql: renderSQLValue,
      json: renderJSONValue,
    };
    const useFormattedValue =
      window &&
      window.sqlFormatter &&
      window.hljs &&
      this.props.textLanguage &&
      ['sql', 'json'].indexOf(this.props.textLanguage.toLowerCase() !== -1); //=== 'sql';

    return (
      <div className={`${style.codeBlockCustom}`} id={`${containerId}`}>
        <div className={`${style.copyGenerated}`}>
          <div className={`${style.copyTooltip}`}>
            <span
              className={toolTipClass ? toolTipClass : style.tooltiptext}
              id={`${id || 'copyCustomFunctionSQL'}`}
            >
              Copy
            </span>
            <i
              className={'fa fa-copy'}
              onClick={this.copyToClip.bind(
                this,
                id || 'copyCustomFunctionSQL'
              )}
              onMouseLeave={this.resetCopy.bind(
                this,
                id || 'copyCustomFunctionSQL'
              )}
            />
            {/*
              onClick={this.copyToClip.bind(this, 'plan', 'copyPlan')}
              onMouseLeave={this.resetCopy.bind(this, 'copyPlan')}
            */}
          </div>
        </div>
        {useFormattedValue
          ? typeRenderer[this.props.textLanguage.toLowerCase()]()
          : renderSimpleValue()}
      </div>
    );
  }
}

TextAreaWithCopy.propTypes = {
  copyText: PropTypes.string.isRequired,
  textLanguage: PropTypes.string,
  id: PropTypes.string,
  containerId: PropTypes.string,
};

export default TextAreaWithCopy;
