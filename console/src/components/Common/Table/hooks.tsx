import React from 'react';

export const TableContext = React.createContext<
  | {
      data: Record<string, any[]>;
      currentCol: number;
      setCurrentCol: (value: React.SetStateAction<number>) => void;
      inputValue: string;
      setInputValue: (value: React.SetStateAction<string>) => void;
      currentRow: string;
      setCurrentRow: (value: React.SetStateAction<string>) => void;
      readonlyRows: string[];
      addReadOnlyRow: (value: string) => void;
      hasLegend: boolean;
      setHasLegend: (value: React.SetStateAction<boolean>) => void;
      showForm: boolean;
      setShowForm: (value: React.SetStateAction<boolean>) => void;
      colKeys: string[];
      setColKeys: (value: React.SetStateAction<string[]>) => void;
      rowKeys: string[];
      setRowKeys: (value: React.SetStateAction<string[]>) => void;
      updateRowKey: (value: string, index: number) => void;
      updateData: (key: string, payload: any[]) => void;
    }
  | undefined
>(undefined);

export function useTable() {
  const context = React.useContext(TableContext);
  if (context === undefined) {
    throw new Error('useTable must be used within a TableProvider');
  }
  return context;
}

export const TableProvider: React.FC = ({ children }) => {
  const [currentCol, setCurrentCol] = React.useState(0);
  const [currentRow, setCurrentRow] = React.useState('---');
  const [inputValue, setInputValue] = React.useState('');
  const [readonlyRows, setReadOnlyRows] = React.useState<string[]>([]);
  const [colKeys, setColKeys] = React.useState<string[]>([]);
  const [rowKeys, setRowKeys] = React.useState<string[]>([]);
  const [data, setData] = React.useState<Record<string, any[]>>({});
  const [hasLegend, setHasLegend] = React.useState(false);
  const [showForm, setShowForm] = React.useState(false);
  const value = {
    data,
    colKeys,
    setColKeys,
    currentCol,
    setCurrentCol,
    currentRow,
    setCurrentRow,
    readonlyRows,
    inputValue,
    setInputValue,
    setReadOnlyRows,
    rowKeys,
    setRowKeys,
    hasLegend,
    setHasLegend,
    showForm,
    setShowForm,
    addReadOnlyRow(col: string) {
      setReadOnlyRows(pre => {
        const next = new Set([...pre, col]);
        return Array.from(next);
      });
    },
    updateRowKey(val: string, index: number) {
      setRowKeys(keys => {
        keys[index] = val;
        return [...keys];
      });
    },
    updateData(key: string, payload: any[]) {
      setData(pre => {
        pre[key] = payload;
        return pre;
      });
    },
  };
  return (
    <TableContext.Provider value={value}>{children}</TableContext.Provider>
  );
};
