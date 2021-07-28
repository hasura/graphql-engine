package util

import (
	"io"

	"github.com/olekukonko/tablewriter"
)

func NewTableWriter(w io.Writer) *tablewriter.Table {
	table := tablewriter.NewWriter(w)

	table.SetHeaderAlignment(tablewriter.ALIGN_LEFT)
	table.SetAlignment(tablewriter.ALIGN_LEFT)
	table.SetBorder(false)
	table.SetRowSeparator("")
	table.SetColumnSeparator("")
	table.SetCenterSeparator("")

	return table
}
