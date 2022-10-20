import { ServerHeader } from '../remoteSchemas/remoteSchemas';

export type ActionName = string;
export type GraphQLType = string;
export type WebhookURL = string;
export type GraphQLName = string;

export interface InputArgument {
  name: string;
  type: GraphQLType;
}

type HeaderKey = string;
type HeaderValue = string;

export type ActionRequestTransform = {
  method?: string;
  url?: string;

  content_type?: string;
  query_params?: Record<string, string>;
  request_headers?: {
    add_headers?: Record<HeaderKey, HeaderValue>;
    remove_headers?: HeaderKey[];
  };
  /**
   * Template language to be used for this transformation. Default: "Kriti"
   */
  template_engine?: 'Kriti';
} & (
  | {
      version?: '1';
      body?: string;
    }
  | {
      version?: '2';
      body?: {
        action: 'remove' | 'transform' | 'x_www_form_urlencoded';
        template?: string;
        form_template?: string;
      };
    }
);

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
  headers?: ServerHeader[];
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
}

export interface Action {
  name: ActionName;
  definition: ActionDefinition;
  comment?: string;
  permissions?: { role: string }[];
}
