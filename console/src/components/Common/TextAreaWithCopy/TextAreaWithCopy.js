import React from 'react';
import sqlFormatter from 'sql-formatter';
import hljs from 'highlight.js';
import PropTypes from 'prop-types';

class TextAreaWithCopy extends React.Component {
  copyToClip(id, e) {
    e.preventDefault();

    const { copyText, textLanguage, containerId } = this.props;

    let text = '';
    if (copyText.length > 0) {
      switch (textLanguage) {
        case 'sql':
          text = sqlFormatter.format(copyText, { language: textLanguage });
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
      return (
        <pre>
          <code
            className={style.formattedCode}
            dangerouslySetInnerHTML={{
              __html: hljs.highlight(
                'sql',
                sqlFormatter.format(copyText, { language: textLanguage })
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
              __html: hljs.highlight(
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
