import React from 'react';

const defaultFoldIconComponent = ({ collapsed, name }) => {
  let icon;
  let title;

  if (collapsed) {
    icon = 'fa-caret-right';
    title = name ? `Expand column "${name}"` : 'Expand column';
  } else {
    icon = 'fa-caret-left';
    title = 'Collapse column';
  }

  return <i className={'fa ' + icon} title={title} />;
};

const defaultFoldButtonComponent = ({ header, collapsed, icon, onClick }) => {
  const style = {
    cursor: 'pointer',
    position: 'absolute',
    fontSize: '14px',
    top: 'calc(50% - 8px)',
    left: '8px',
    padding: '5px',
    margin: '-5px -5px -5px -5px',
  };

  return (
    <div>
      <div style={style} onClick={onClick}>
        {icon}
      </div>
      {!collapsed && <div>{header}</div>}
    </div>
  );
};

export default ReactTable => {
  const wrapper = class RTFoldableTable extends React.Component {
    constructor(props, context) {
      super(props, context);

      this.state = {
        folded: props.onFoldChange ? undefined : {},
        resized: props.resized || [],
      };
    }

    UNSAFE_componentWillReceiveProps(newProps) {
      if (this.state.resized !== newProps.resized) {
        this.setState({ resized: newProps.resized });
      }
    }

    onResizedChange = resized => {
      const { onResizedChange } = this.props;
      if (onResizedChange) {
        onResizedChange(resized);
      } else {
        this.setState({ resized });
      }
    };

    removeResized = ({ id }) => {
      if (!id) return;

      const { resized } = this.state;
      if (!resized) return;

      const rs = resized.find(r => r.id === id);
      if (!rs) return;

      const newResized = resized.filter(r => r !== rs);
      this.onResizedChange(newResized);
    };

    // this is so we can expose the underlying ReactTable.
    getWrappedInstance = () => {
      if (!this.wrappedInstance) {
        console.warn('RTFoldableTable - No wrapped instance');
      }
      if (this.wrappedInstance.getWrappedInstance) {
        return this.wrappedInstance.getWrappedInstance();
      }
      return this.wrappedInstance;
    };

    getCopiedKey = key => {
      const { foldableOriginalKey } = this.props;
      return `${foldableOriginalKey}${key}`;
    };

    copyOriginals = column => {
      const { FoldedColumn } = this.props;

      //Stop copy if the column already copied
      if (column.original_Header) return;

      Object.keys(FoldedColumn).forEach(k => {
        const copiedKey = this.getCopiedKey(k);

        if (k === 'Cell') {
          column[copiedKey] = column[k] ? column[k] : c => c.value;
        } else column[copiedKey] = column[k];
      });

      //Copy sub Columns
      if (column.columns && !column.original_Columns) {
        column.original_Columns = column.columns;
      }

      //Copy Header
      if (!column.original_Header) {
        column.original_Header = column.Header;
      }
    };

    restoreToOriginal = column => {
      const { FoldedColumn } = this.props;

      Object.keys(FoldedColumn).forEach(k => {
        //ignore header as handling by foldableHeaderRender
        if (k === 'Header') return;

        const copiedKey = this.getCopiedKey(k);
        column[k] = column[copiedKey];
      });

      if (column.columns && column.original_Columns) {
        column.columns = column.original_Columns;
      }
    };

    getFoldedState = () => {
      return this.props.onFoldChange ? this.props.folded : this.state.folded;
    };

    isFolded = col => {
      const folded = this.getFoldedState();
      return folded[col.id] === true;
    };

    foldingHandler = col => {
      if (!col || !col.id) return;

      const { onFoldChange } = this.props;
      const folded = this.getFoldedState();
      const { id } = col;

      const newFold = Object.assign({}, folded);
      newFold[id] = !newFold[id];

      //Remove the Resized if have
      this.removeResized(col);

      if (onFoldChange) {
        onFoldChange(newFold);
      } else this.setState({ folded: newFold });
    };

    foldableHeaderRender = ({ column }) => {
      const { FoldButtonComponent, FoldIconComponent } = this.props;
      const collapsed = this.isFolded(column);
      const icon = <FoldIconComponent collapsed={collapsed} name={column.id} />;
      const onClick = e => {
        e.stopPropagation();
        this.foldingHandler(column);
      };

      return (
        <FoldButtonComponent
          header={column.original_Header}
          collapsed={collapsed}
          icon={icon}
          onClick={onClick}
        />
      );
    };

    applyFoldableForColumn = column => {
      const collapsed = this.isFolded(column);
      const { FoldedColumn } = this.props;

      if (collapsed) {
        if (column.columns) {
          column.columns = [FoldedColumn];
          column.width = FoldedColumn.width;
          column.style = FoldedColumn.style;
        } else {
          Object.assign(column, FoldedColumn);
        }
      } else this.restoreToOriginal(column);
    };

    applyFoldableForColumns = columns => {
      return columns.filter(Boolean).map((col, index) => {
        if (!col.foldable) return col;

        //If col don't have id then generate id based on index
        if (!col.id) {
          col.id = `col_${index}`;
        }

        this.copyOriginals(col);
        //Replace current header with internal header render.
        col.Header = c => this.foldableHeaderRender(c);
        //apply foldable
        this.applyFoldableForColumn(col);

        //return the new column out
        return col;
      });
    };

    render() {
      const { columns: originalCols, ...rest } = this.props;
      const columns = this.applyFoldableForColumns([...originalCols]);

      const extra = {
        columns,
        onResizedChange: this.onResizedChange,
        resized: this.state.resized,
      };

      return (
        <ReactTable
          {...rest}
          {...extra}
          ref={r => (this.wrappedInstance = r)}
        />
      );
    }
  };

  wrapper.displayName = 'RTFoldableTable';
  wrapper.defaultProps = {
    FoldIconComponent: defaultFoldIconComponent,
    FoldButtonComponent: defaultFoldButtonComponent,
    foldableOriginalKey: 'original_',
    FoldedColumn: {
      Cell: () => '',
      width: 22,
      headerClassName: 'collapsed',
      sortable: false,
      resizable: false,
      filterable: false,
    },
  };

  return wrapper;
};
