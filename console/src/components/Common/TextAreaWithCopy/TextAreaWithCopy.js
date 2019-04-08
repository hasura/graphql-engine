import React from 'react';

import PropTypes from 'prop-types';

class TextAreaWithCopy extends React.Component {
  copyToClip(type, id) {
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

    const textArea = document.createElement('textarea');
    textArea.value = text;
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      const successful = document.execCommand('copy');
      // const msg = successful ? 'successful' : 'unsuccessful';
      const tooltip = document.getElementById(id);
      tooltip.innerHTML = 'Copied';
      if (!successful) {
        throw new Error('Copy was unsuccessful');
      }
    } catch (err) {
      alert('Oops, unable to copy - ' + err);
    }
    document.body.removeChild(textArea);
  }

  resetCopy(id) {
    const tooltip = document.getElementById(id);
    tooltip.innerHTML = 'Copy';
  }

  render() {
    const style = require('./TextAreaWithCopy.scss');

    const { copyText, toolTipClass } = this.props;

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

    const useSQLValue =
      window &&
      window.sqlFormatter &&
      window.hljs &&
      this.props.textLanguage &&
      this.props.textLanguage.toLowerCase() === 'sql';

    return (
      <div className={`${style.codeBlockCustom}`}>
        <div className={`${style.copyGenerated}`}>
          <div className={`${style.copyTooltip}`}>
            <span
              className={toolTipClass ? toolTipClass : style.tooltiptext}
              id="copyCustomFunctionSQL"
            >
              Copy
            </span>
            <i
              className={'fa fa-copy'}
              onClick={this.copyToClip.bind(
                this,
                'plan',
                'copyCustomFunctionSQL'
              )}
              onMouseLeave={this.resetCopy.bind(this, 'copyCustomFunctionSQL')}
            />
            {/*
              onClick={this.copyToClip.bind(this, 'plan', 'copyPlan')}
              onMouseLeave={this.resetCopy.bind(this, 'copyPlan')}
            */}
          </div>
        </div>

        {useSQLValue ? renderSQLValue() : renderSimpleValue()}
      </div>
    );
  }
}

TextAreaWithCopy.propTypes = {
  copyText: PropTypes.string.isRequired,
  textLanguage: PropTypes.string,
};

export default TextAreaWithCopy;
