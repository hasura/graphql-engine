/* eslint-disable */

import React, { Component } from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';
import FoldableHoc from './foldableTable';

import { isObject, isNotDefined } from '../utils/jsUtils';

class DragFoldTable extends Component {
  constructor(props) {
    super(props);
    this.dragged = null;
    this.reorders = props.defaultReorders || [];
    this.state = {
      trigger: 0,
      folded: props.defaultCollapsed || {},
    };
    this.tableRef = React.createRef();
  }
  mountEvents() {
    if (!this.tableRef.current) return;

    const headers = Array.prototype.slice.call(
      this.tableRef.current.querySelectorAll('.draggable-header')
    );

    headers.forEach((header, i) => {
      header.setAttribute('draggable', true);
      //the dragged header
      header.ondragstart = e => {
        e.stopPropagation();
        this.dragged = i;
      };

      header.ondrag = e => e.stopPropagation;

      header.ondragend = e => {
        e.stopPropagation();
        setTimeout(() => (this.dragged = null), 1000);
      };

      //the dropped header
      header.ondragover = e => {
        e.preventDefault();
      };

      header.ondrop = e => {
        e.preventDefault();
        if (this.dragged) {
          this.reorders.push({
            newOrder: i,
            defaultOrder: this.dragged,
          });
        }
        if (this.props.onOrderChange) {
          this.props.onOrderChange(this.reorders);
        }
        this.setState({ trigger: Math.random() });
      };
    });
  }
  componentDidMount() {
    this.mountEvents();
  }

  componentDidUpdate() {
    this.mountEvents();
  }
  render() {
    const { data, columns, headerTitle } = this.props;

    const cols = columns.map((col, idx) => ({
      ...col,
      Header: (
        <div
          className="draggable-header"
          title={headerTitle || 'Drag to rearrange'}
        >
          {col.Header}
        </div>
      ),
    }));

    //run all reorder events
    this.reorders.forEach(o =>
      cols.splice(o.newOrder, 0, cols.splice(o.defaultOrder, 1)[0])
    );

    const FoldableTable = FoldableHoc(ReactTable);

    //render
    return (
      <div className="esr-table" ref={this.tableRef}>
        <FoldableTable
          {...this.props}
          data={data}
          columns={cols}
          onFoldChange={newFolded => {
            if (this.props.onCollapseChange) {
              this.props.onCollapseChange(newFolded);
            }
            this.setState({ folded: newFolded });
          }}
          folded={this.state.folded}
        />
      </div>
    );
  }
}

// return column width to fit all rows content
export const getColWidth = (
  header,
  contentRows = [],
  MAX_WIDTH = 600,
  HEADER_PADDING = 62,
  CONTENT_PADDING = 18,
  HEADER_FONT = 'bold 16px Gudea',
  CONTENT_FONT = '14px Gudea'
) => {
  const getTextWidth = (text, font) => {
    // Doesn't work well with non-monospace fonts
    // const CHAR_WIDTH = 8;
    // return text.length * CHAR_WIDTH;

    // if given, use cached canvas for better performance
    // else, create new canvas
    const canvas =
      getTextWidth.canvas ||
      (getTextWidth.canvas = document.createElement('canvas'));

    const context = canvas.getContext('2d');
    context.font = font;

    const metrics = context.measureText(text);
    return metrics.width;
  };

  let maxContentWidth = 0;
  for (let i = 0; i < contentRows.length; i++) {
    if (contentRows[i] !== undefined && contentRows[i][header] !== null) {
      const content = contentRows[i][header];

      let contentString;
      if (isNotDefined(content)) {
        contentString = 'NULL';
      } else if (isObject(content)) {
        contentString = JSON.stringify(content, null, 4);
      } else {
        contentString = content.toString();
      }

      const currLength = getTextWidth(contentString, CONTENT_FONT);

      if (currLength > maxContentWidth) {
        maxContentWidth = currLength;
      }
    }
  }

  const maxContentCellWidth = maxContentWidth + CONTENT_PADDING;

  const headerCellWidth = getTextWidth(header, HEADER_FONT) + HEADER_PADDING;

  return Math.min(MAX_WIDTH, Math.max(maxContentCellWidth, headerCellWidth));
};

export default DragFoldTable;
