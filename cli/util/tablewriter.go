package util

import (
	"io"

	"github.com/olekukonko/tablewriter"
	"github.com/olekukonko/tablewriter/tw"
)

func NewTableWriter(w io.Writer) *tablewriter.Table {
	table := tablewriter.NewTable(w, tablewriter.WithConfig(tablewriter.Config{
		Header: tw.CellConfig{
			Formatting: tw.CellFormatting{AutoFormat: tw.On},
			Alignment:  tw.CellAlignment{Global: tw.AlignLeft},
		},
		Row: tw.CellConfig{
			Alignment: tw.CellAlignment{Global: tw.AlignLeft},
		},
	}), tablewriter.WithRendition(tw.Rendition{
		Borders: tw.BorderNone,
	}))

	return table
}
