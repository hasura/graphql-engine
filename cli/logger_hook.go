package cli

import (
	"github.com/briandowns/spinner"
	"github.com/mattn/go-colorable"
	"github.com/sirupsen/logrus"
)

type loggerHook struct {
	logger  *logrus.Logger
	spinner *spinner.Spinner
}

func newLoggerHook(parent *logrus.Logger, spinner *spinner.Spinner, isTerminal, noColor bool) *loggerHook {
	logger := logrus.New()
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
	} else {
		logger.Formatter = &logrus.JSONFormatter{
			PrettyPrint: false,
		}
	}
	logger.Out = colorable.NewColorableStdout()
	logger.Level = parent.GetLevel()
	return &loggerHook{
		logger:  logger,
		spinner: spinner,
	}
}

// Levels returns all levels this hook should be registered to
func (hook *loggerHook) Levels() []logrus.Level {
	return logrus.AllLevels
}

// Fire is triggered on new log entries
func (hook *loggerHook) Fire(entry *logrus.Entry) error {
	if hook.spinner.Active() {
		hook.spinner.Stop()
		defer func() {
			hook.spinner.Start()
		}()
	}
	entry.Logger = hook.logger
	return nil
}
