/* eslint-disable */

import React, { Component } from 'react';
import ReactTable from 'react-table';
import 'react-table/react-table.css';

import FoldableHoc from './foldableTable';

class DragFoldTable extends Component {
  constructor(props) {
    super(props);
    this.dragged = null;
    this.reorder = [];
    this.state = {
      trigger: 0,
      folded: {},
    };
  }
  mountEvents() {
    const headers = Array.prototype.slice.call(
      document.querySelectorAll('.draggable-header')
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
          this.reorder.push({ a: i, b: this.dragged });
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
    const { data, columns } = this.props;

    const cols = columns.map(col => ({
      ...col,
      Header: <div className="draggable-header">{col.Header}</div>,
    }));

    //run all reorder events
    this.reorder.forEach(o => cols.splice(o.a, 0, cols.splice(o.b, 1)[0]));

    const FoldableTable = FoldableHoc(ReactTable);

    //render
    return (
      <div className="esr-table">
        <FoldableTable
          {...this.props}
          data={data}
          columns={cols}
          onFoldChange={newFolded =>
            this.setState(p => {
              return { folded: newFolded };
            })
          }
          folded={this.state.folded}
        />
      </div>
    );
  }
}

export default DragFoldTable;
