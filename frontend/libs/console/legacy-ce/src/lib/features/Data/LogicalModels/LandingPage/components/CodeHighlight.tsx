export const CodeHighlight: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => (
  <pre className="inline-block bg-slate-100 p-[1px_6px] rounded-md">
    {children}
  </pre>
);
