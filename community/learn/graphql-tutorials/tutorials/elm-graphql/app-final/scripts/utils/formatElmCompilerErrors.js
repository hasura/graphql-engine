'use strict';

var chalk = require('chalk');
var ctx = new chalk.constructor({ enabled: true });
var error = ctx.bold.red;
var filename = ctx.cyan;
var isBrowser = typeof window === 'object';

if (isBrowser) {
  ctx.level = 1;
  error = ctx.cyan;
  filename = ctx.green;
}

function stripRedundantInfo(error) {
  return (
    error
      // String the error message from the loader.
      .replace(/Module build failed.*\nError.*\n/gm, '')
      // Strip compilation progress-bar.
      .replace(/\[[=\s]{3,}\]\s-\s\d+\s\/\s\d+[\r\n\s]+/gm, '')
  );
}

module.exports = function formatElmCompilerErrors(messages) {
  var errors = messages.errors;
  var warnings = messages.warnings;
  return errors.length > 0
    ? {
        errors: errors
          .map(x =>
            x
              .replace(/(--\s[A-Z\s]+-+\s.*\.elm\r?\n)/g, filename('$1'))
              .replace(/(\n\s*)(\^+)/g, '$1' + error('$2'))
              .replace(/(\d+)(\|>)/g, '$1' + error('$2'))
          )
          .map(stripRedundantInfo)
          // drop errors that only contain whitespace
          .filter(err => err.trim()),
        warnings: warnings
      }
    : messages;
};
