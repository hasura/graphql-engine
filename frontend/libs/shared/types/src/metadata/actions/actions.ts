import { HeaderConfig } from '../header';

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#requesttransformation
 */
export type RequestTransformMethod =
  'POST' | 'GET' | 'PUT' | 'DELETE' | 'PATCH';

export type RequestTransformContentType =
  'application/json' | 'application/x-www-form-urlencoded';

export type RequestTransformBodyActions =
  'remove' | 'transform' | 'x_www_form_urlencoded';

export type RequestTransformBody = {
  action: RequestTransformBodyActions;
  template?: string;
  form_template?: Record<string, string> | string;
};

export type ResponseTransformBody = {
  action: RequestTransformBodyActions;
  template?: string;
};

export type RequestTransformHeaders = {
  add_headers?: Record<string, string>;
  remove_headers?: string[];
};

export type RequestTransformTemplateEngine = 'Kriti';

export type ResponseTransform = {
  version: 2;
  body?: ResponseTransformBody;
  template_engine?: RequestTransformTemplateEngine | null;
  comment?: string;
};

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#inputargument
 */
export interface InputArgument {
  name: string;
  type: GraphQLType;
}

export type ActionName = string;
export type GraphQLType = string;
export type WebhookURL = string;
export type GraphQLName = string;

type HeaderKey = string;
type HeaderValue = string;

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs/#requesttransformation
 */
export type ActionRequestTransform = {
  method?: RequestTransformMethod | null;
  url?: string;

  content_type?: string;
  query_params?: Record<string, string> | string | null;
  request_headers?: {
    add_headers?: Record<HeaderKey, HeaderValue>;
    remove_headers?: HeaderKey[];
  };
  /**
   * Template language to be used for this transformation. Default: "Kriti"
   */
  template_engine?: RequestTransformTemplateEngine;
} & (
  | {
      version: 1;
      body?: string;
    }
  | {
      version: 2;
      body?: RequestTransformBody;
    }
);

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#actiondefinition
 */
export interface ActionDefinition {
  /**
   * Input arguments
   */
  arguments?: InputArgument[];
  /**
   * The output type of the action. Only object and list of objects are allowed.
   */
  output_type: string;
  /**
   * The kind of the mutation action (default: synchronous). If the type of the action is query then the kind field should be omitted.
   */
  kind?: 'synchronous' | 'asynchronous';
  /**
   * List of defined headers to be sent to the handler
   */
  headers?: HeaderConfig[];
  /**
   * If set to true the client headers are forwarded to the webhook handler (default: false)
   */
  forward_client_headers: boolean;
  /**
   * The action's webhook URL
   */
  handler: WebhookURL;
  /**
   * The type of the action (default: mutation)
   */
  type?: 'mutation' | 'query';

  /**
   * Request Transformation to be applied to this Action's request
   */
  request_transform?: ActionRequestTransform;
  /**
   * Response Transformation to be applied to this Action's response
   */
  response_transform?: ResponseTransform;
  /**
   * Request timeout.
   */
  timeout?: number;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#args-syntax
 */
export interface Action {
  /** Name of the action  */
  name: ActionName;
  /** Definition of the action */
  definition: ActionDefinition;
  /** Comment */
  comment?: string;
  /** Permissions of the action */
  permissions?: Array<{ role: string }>;
}
