package cli

import (
	"io"

	"github.com/briandowns/spinner"
	"github.com/mattn/go-colorable"
	"github.com/sirupsen/logrus"
)

type spinnerHook struct {
	logger  *logrus.Logger
	spinner *spinner.Spinner
}

func newSpinnerHandlerHook(parent *logrus.Logger, spinner *spinner.Spinner, isTerminal, noColor bool) *spinnerHook {
	logger := logrus.New()
	logger.Out = parent.Out
	if parent.Out != io.Discard {
		if isTerminal {
			if noColor {
				logger.Formatter = &logrus.TextFormatter{
					DisableColors:    true,
					DisableTimestamp: true,
				}
			} else {
				logger.Formatter = &logrus.TextFormatter{
					ForceColors:      true,
					DisableTimestamp: true,
				}
			}
			logger.Out = colorable.NewColorableStderr()
		} else {
			logger.Formatter = &logrus.JSONFormatter{
				PrettyPrint: false,
			}
		}
		logger.Level = parent.GetLevel()
	}
	return &spinnerHook{
		logger:  logger,
		spinner: spinner,
	}
}

// Levels returns all levels this hook should be registered to
func (hook *spinnerHook) Levels() []logrus.Level {
	return logrus.AllLevels
}

// Fire is triggered on new log entries
func (hook *spinnerHook) Fire(entry *logrus.Entry) error {
	if hook.spinner.Active() {
		hook.spinner.Stop()
		defer func() {
			hook.spinner.Start()
		}()
	}
	entry.Logger = hook.logger
	return nil
}
