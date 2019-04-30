import * as React from "react";
import Highlight, { defaultProps } from "prism-react-renderer";
import prismTheme from "prism-react-renderer/themes/vsDark";
import { LiveProvider, LiveEditor, LiveError, LivePreview } from "react-live";
import '../styles.css';

import Pre from "./pre";

/** Removes the last token from a code example if it's empty. */
function cleanTokens(tokens) {
  const tokensLength = tokens.length;
  if (tokensLength === 0) {
    return tokens;
  }
  const lastToken = tokens[tokensLength - 1];
  if (lastToken.length === 1 && lastToken[0].empty) {
    return tokens.slice(0, tokensLength - 1);
  }
  return tokens;
}

/* eslint-disable react/jsx-key */
const CodeBlock = ({ children: exampleCode, ...props }) => {
  if (props["react-live"]) {
    return (
      <LiveProvider code={exampleCode}>
        <LiveEditor />
        <LiveError />
        <LivePreview />
      </LiveProvider>
    );
  } else {
    return (
      <Highlight
        {...defaultProps}
        code={exampleCode}
        language="javascript"
        theme={prismTheme}
      >
        {({ className, style, tokens, getLineProps, getTokenProps }) => (
          <Pre className={className} style={style} p={3}>
            {cleanTokens(tokens).map((line, i) => {
              let lineClass = {};
              let isDiff = false;
              if (line[0] && line[0].content.length && line[0].content[0] === '+') {
                lineClass = {'backgroundColor': 'rgba(76, 175, 80, 0.2)'};
                isDiff = true;
              }
              else if (line[0] && line[0].content.length && line[0].content[0] === '-') {
                lineClass = {'backgroundColor': 'rgba(244, 67, 54, 0.2)'};
                isDiff = true;
              }
              else if(line[0] && line[0].content === '' && line[1] && line[1].content === '+') {
                lineClass = {'backgroundColor': 'rgba(76, 175, 80, 0.2)'};
                isDiff = true;
              } else if(line[0] && line[0].content === ''&& line[1] && line[1].content === '-') {
                lineClass = {'backgroundColor': 'rgba(244, 67, 54, 0.2)'};
                isDiff = true;
              }
              const lineProps = getLineProps({line, key: i});
              lineProps.style = lineClass;
              const diffStyle = {'userSelect': 'none', '-moz-user-select': '-moz-none'};
              let splitToken;
              return (
                <div {...lineProps}>
                  {line.map((token, key) => {
                    if(isDiff) {
                      if ((key === 0 || key === 1) & (token.content.charAt(0) === '+' || token.content.charAt(0) === '-')) {
                        if(token.content.length > 1) {
                          splitToken = { 'types': ['template-string','string'], 'content': token.content.slice(1)};
                          const firstChar = { 'types': ['operator'], 'content': token.content.charAt(0)};
                          return (
                            <React.Fragment>
                              <span {...getTokenProps({ token: firstChar, key })} style={diffStyle} />
                              <span {...getTokenProps({ token: splitToken, key })} />
                            </React.Fragment>
                          )
                        } else {
                          return (
                              <span {...getTokenProps({ token, key })} style={diffStyle} />
                          )

                        }
                      }
                    }
                    return (<span {...getTokenProps({ token, key })} />)
                   } )}
                </div>
            )})}
          </Pre>
        )}
      </Highlight>
    );
  }
};

export default CodeBlock;
