export const Token = ({ token, inline }: { token: any; inline?: boolean }) => {
  return (
    <div
      className={`font-bold text-lg text-black ${inline ? 'inline-block' : ''}`}
    >
      {token}
    </div>
  );
};
