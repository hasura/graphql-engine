import React from 'react';

import PropTypes from 'prop-types';

class TextAreaWithCopy extends React.Component {
  copyToClip(id, e) {
    e.preventDefault();

    const { copyText, textLanguage, containerId } = this.props;

    let text = '';
    if (copyText.length > 0) {
      switch (textLanguage) {
        case 'sql':
          text = window.sqlFormatter
            ? window.sqlFormatter.format(copyText, { language: textLanguage })
            : copyText;
          break;
        default:
          text = copyText;
      }
    }

    const textArea = document.createElement('textarea');
    textArea.value = text;

    const appendLoc = containerId
      ? document.getElementById(containerId)
      : document.body;

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

    const {
      copyText,
      toolTipClass,
      id,
      containerId,
      textLanguage,
    } = this.props;

    const renderSimpleValue = () => {
      return (
        <pre className={style.schemaPreWrapper}>
          <code className={style.formattedCode}>{copyText}</code>
        </pre>
      );
    };

    const renderSQLValue = () => {
      if (!window || !window.hljs || !window.sqlFormatter) {
        return renderSimpleValue();
      }

      return (
        <pre>
          <code
            className={style.formattedCode}
            dangerouslySetInnerHTML={{
              __html: window.hljs.highlight(
                'sql',
                window.sqlFormatter.format(copyText, { language: textLanguage })
              ).value,
            }}
          />
        </pre>
      );
    };

    const renderJSONValue = () => {
      if (!window || !window.hljs) {
        return renderSimpleValue();
      }

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

    const getTypeRenderer = type => {
      let typeRenderer;

      switch (type) {
        case 'sql':
          typeRenderer = renderSQLValue;
          break;
        case 'json':
          typeRenderer = renderJSONValue;
          break;
        default:
          typeRenderer = renderSimpleValue;
      }

      return typeRenderer;
    };

    return (
      <div className={style.codeBlockCustom} id={containerId}>
        <div className={style.copyGenerated}>
          <div className={style.copyTooltip}>
            <span
              className={toolTipClass ? toolTipClass : style.tooltiptext}
              id={id}
            >
              Copy
            </span>
            <i
              className={'fa fa-copy'}
              onClick={this.copyToClip.bind(this, id)}
              onMouseLeave={this.resetCopy.bind(this, id)}
            />
          </div>
        </div>
        {getTypeRenderer(textLanguage)()}
      </div>
    );
  }
}

TextAreaWithCopy.propTypes = {
  copyText: PropTypes.string.isRequired,
  textLanguage: PropTypes.string,
  id: PropTypes.string.isRequired,
  containerId: PropTypes.string,
};

export default TextAreaWithCopy;
