import React from 'react';

import PropTypes from 'prop-types';

class ReusableTextAreaWithCopy extends React.Component {
  copyToClip(type, id) {
    let text = '';
    if (this.props.copyText.length > 0) {
      text = window.sqlFormatter
        ? window.sqlFormatter.format(this.props.copyText, {
          language: this.props.textLanguage,
        })
        : this.props.copyText;
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
    const style = require('./style.scss');
    const { copyText } = this.props;
    return (
      <div className={`${style.codeBlockCustom}`}>
        <div className={`${style.copyGenerated}`}>
          <div className={`${style.copyTooltip}`}>
            <span className={style.tooltiptext} id="copyCustomFunctionSQL">
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

        {window && window.sqlFormatter && window.hljs ? (
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
        ) : (
          <pre>
            <code className={style.formattedCode}>{copyText}</code>
          </pre>
        )}
      </div>
    );
  }
}

ReusableTextAreaWithCopy.propTypes = {
  copyText: PropTypes.string.isRequired,
  textLanguage: PropTypes.string,
};

export default ReusableTextAreaWithCopy;
