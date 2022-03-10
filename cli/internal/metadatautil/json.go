package metadatautil

import (
	"bufio"
	"bytes"
	"fmt"
	"os"

	"github.com/mikefarah/yq/v4/pkg/yqlib"
	logging "gopkg.in/op/go-logging.v1"
)

func JSONToYAML(bs []byte) ([]byte, error) {
	var format = logging.MustStringFormatter(
		`%{color}%{time:15:04:05} %{shortfunc} [%{level:.4s}]%{color:reset} %{message}`,
	)
	var backend = logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), format))
	backend.SetLevel(logging.ERROR, "")
	yqlib.GetLogger().SetBackend(backend)

	yqlib.InitExpressionParser()
	decoder := yqlib.NewYamlDecoder()
	encoder := yqlib.NewYamlEncoder(2, false, false, true)
	var buf bytes.Buffer
	se := yqlib.NewStreamEvaluator()
	pw := yqlib.NewSinglePrinterWriter(&buf)
	p := yqlib.NewPrinter(encoder, pw)
	expression := fmt.Sprintf("%v | %v", ".", yqlib.PrettyPrintExp)
	en, err := yqlib.ExpressionParser.ParseExpression(expression)
	if err != nil {
		return nil, err
	}
	_, err = se.Evaluate("sample.json", bufio.NewReader(bytes.NewReader(bs)), en, p, "", decoder)
	if err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}
