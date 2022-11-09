import React from 'react';

const gqlRegex = new RegExp('[^A-Za-z0-9_]', 'g');

export const sanitizeGraphQLFieldNames = (value: string): string => {
  return value.replace(' ', '_').replace(gqlRegex, '');
};

export const SanitizeTips: React.VFC = () => (
  <div className="flex text-muted flex-col mb-4">
    <div>GraphQL fields are limited to letters, numbers, and underscores.</div>
    <div>Any spaces are converted to underscores.</div>
  </div>
);
