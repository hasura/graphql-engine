import React from 'react';

type NeonIconProps = {
  className?: string;
};

export function NeonIcon(props: NeonIconProps) {
  const { className } = props;
  return (
    <svg
      width="13"
      height="13"
      viewBox="0 0 13 13"
      fill="none"
      className={className}
      xmlns="http://www.w3.org/2000/svg"
    >
      <rect x="0.75" y="0.5" width="12" height="12" fill="url(#pattern0)" />
      <defs>
        <pattern
          id="pattern0"
          patternContentUnits="objectBoundingBox"
          width="1"
          height="1"
        >
          <use xlinkHref="#image0_445_24578" transform="scale(0.03125)" />
        </pattern>
        <image
          id="image0_445_24578"
          width="32"
          height="32"
          xlinkHref="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAFW0lEQVRYhbWXUYhdVxWGv3+dM8lMZhITqK1Ea1Nq+xKhIIgPKibUBxNEFDMgiDVpIKDFKj5ZETwPgr4IrVDEakyIVKETRRDaR8cHn6rQImnSQjGoYJNQWpJpJnPv2ev34ZwzcydN0pmoCy5n33v22f9//rX+tfcVdiAl/3z5ftp4POQDge8KJYGpsIZxkISuu/Zj0c9Rdh8nUVpUxlZpoW25b+cLF3bvPPf88squH53acerVxk0IgPOvHMQ+HfJMkFSybw7Yg1x/n0Src5NwQW1LlDGRI9qVmt3bX9Z9d/yVNrcsJxw6Ofvsc+K1sw8QejHkmUo5CqiC9MbAJ3/rxhrG2aK2RWVMlDFlFOzecVZ77nixlJzaAixnlQ/WUek7wjOhHAWeClLVBt9aNyJDIgqRBblFLoQLdoKNRIBG4JkoerwWeaB7kKoiFfarAU+hLALJtioIzBAiUSSRUEUHCkkwzDNyAiYyqWir5XZuadvU5S+WrA6Co1/oQF11BefA7qV86vI9H/kJ/4fYP/oco/HWg5LBsuGuejKPFSbkAsDf/zjNHtpbLbhvI6iLwJ4/1Yv3NtdW2um5UGKLTiVRB6ZaX0ydM/bQov23JLC4EQJA430sAjLFCMnYwjJRKbXqZ3J9sv/nUQADvQJIsVrV6q5TpWxuTaPGTQxfJ8cbiXqwnDAVCdXmwBFuaPwNH9i6wt3ZqBlP3ntXApPttLPT5sAPv/n5nbGl/sHVq/6M/UY5+vahZ+/e9uHvN2pyIyTq9R0vN0K6i35itXXqd8B+EBIYvvePq3/7AHBkI8vE6uYxtNB4dxWGPD/y9qEDtvcDY9vFdhEeizh89Or8NxFuvK8+w3tv+lZRsX5XG9JwK4+f4Yw6EfjgsA4QkipMZYHNE4eXD32y0WI7y+zULRWo+iIMcpM1GDlYCoRtu2vfRTKR+s1XrnzhzpM6ee3mBPrc18q+IW3Ghtf5ug9DYLWS3z8V9TPDbKnf/tcR0FoT6hTYhBOo1oHbXrG91JFwAC3w6UeW5n888cANFGCiCDdFYDVsG0lv2TzWj8MmJCfi20eXDj1EcunGBLRGgk2lYIhVZbcvzeqUzHd7EmkDMkjHrfzUgLtGQNcpUN2OAqtFmLuu1LuOz53+odEfgBpITCLuAR0DsiMgAL9DgWqze8GEApJY1rUAmB5PfRXna5JqwDaW5C41toRsTxZh6Y5St6XAmhJyl8Of7vz1m4Yv2W4lVX2Z0IFL/XWtCIfD5H8Tnuh3x3xs6pezv/2L4Wt9PXjCLfQcHEF6fR207/DqRt4cuhQM8TRPt42bODF7+heGnxlXksa2W2AFCMRL9bAPaFAiSrfa+fP1Pjc3hFvijYrOLtVwuulorG/5DY0Blrbp0e3L/pDth3qSte1lQo+tbscaFOi78eK9R27aPukaDFV97UpptyL1wBbTTK/JIIzRghbKvOcPbl/215P8qJLXM/n5ydnT5+pQuSD8vqBIXS08+omLJ4mgdOmwK5KIAiSRY5RtNbq25crlpXOfnZu5hD1o7zIqy+vPkWskRsAT6xRyE3WQz4s8EsoiMoJ8QOEnh1OS+vQwdEkbMqm1zLidRkrblTsBdPbkrt+/tQo8SQKY93y1l70+wxntZa8bNamPXXzm/kq8JOVMOEdSVqG0KKvuWK0PFyhjVFpybOa2XtCuHf8iszJQK3j4+MzCr+Y9Xy1oYUMNRQAfv3jqYIRPB2WmB/SkNUXp/g3lQGBMjszc9CXtfM+/yQxsnjwxt/CtjYBORuAm/nznw88p88GQTwR+fQ28IIpCqaBIbiUXuv2lSGpH4Bdkvnw74AD/AfcZPcz7zebfAAAAAElFTkSuQmCC"
        />
      </defs>
    </svg>
  );
}
