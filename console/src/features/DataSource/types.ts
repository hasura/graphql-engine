export type Ref = { $ref: string };

export type OneOf = { oneOf: (Property | Ref)[]; description?: string };

export type Property = {
  description?: string;
  nullable: boolean;
} & (
  | {
      type: 'object';
      properties: Record<string, Ref | Property | OneOf>;
    }
  | {
      type: 'string';
      enum?: string[];
    }
  | {
      type: 'number';
    }
  | {
      type: 'boolean';
    }
);
