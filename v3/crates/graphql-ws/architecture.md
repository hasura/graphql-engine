# Architecture of GraphQL over WebSockets

## WebSocket Handling Flow

### 1. Client Interaction

- **Initiates WebSocket Request**
  - Client sends a request to establish a WebSocket connection.

### 2. WebSocket Server

- **Validates Protocol**
  - Ensures the incoming request adheres to the expected `graphql-transport-ws`
    protocol.

### 3. Connection Establishment

- **WebSocket Connection**
  - Successfully establishes a connection after protocol validation.
  - **Spawns Tasks:**
    - **InitCheckerTask**: Verifies the protocol's initialization state.
    - **IncomingTask**: Handles incoming client messages.
    - **OutgoingTask**: Sends messages to the client.

### 4. Task Management

- **InitCheckerTask**
  - Checks initialization state and directs to Task Management.
- **IncomingTask**
  - **Client Message Handler**:
    - Deserializes client messages into appropriate types.
    - Routes valid messages to the **ProtocolHandler**.
    - Handles invalid messages by sending error responses.

### 5. Protocol Handling

- **ProtocolHandler**
  - Manages various client messages:
    - **ConnectionInit**: Initializes connection.
    - **Subscribe**: Starts polling for subscriptions.
    - **Complete**: Stops the corresponding poller.
    - **Ping/Pong**: Manages keep-alive messages.

### 6. Polling Mechanism

- **Polling Loop**
  - Fetches data from the data connector.
  - Processes responses:
    - Sends GraphQL responses or errors back to the client.
    - Repeats fetching at defined intervals.

## Diagram

```mermaid
graph TD
    Client -->|Initiates WebSocket Request| WebSocketServer
    WebSocketServer -->|Validates Protocol| ProtocolValidation
    ProtocolValidation -->|Success| WebSocketConnection
    WebSocketConnection -->|Spawn Tasks| InitCheckerTask & IncomingTask & OutgoingTask

    InitCheckerTask -->|Checks Init State| InitState
    InitState -->|Timeout or Success| TaskManagement

    IncomingTask -->|Handles Client Messages| ClientMessageHandler
    OutgoingTask -->|Sends Messages to Client| ClientMessageSender

    ClientMessageHandler -->|Deserializes ClientMessage| DeserializeMessage
    DeserializeMessage -->|Valid ClientMessage| ProtocolHandler
    DeserializeMessage -->|Invalid Message| SendErrorMessage

    TaskManagement -->|Terminate Remaining Tasks| TaskAborter
    ProtocolHandler -->|Process Client Message| HandleClientMessage

    subgraph WebSocketServer
        ProtocolValidation
        WebSocketConnection
        InitCheckerTask
        IncomingTask
        OutgoingTask
    end

    subgraph Tasks
        InitCheckerTask
        IncomingTask
        OutgoingTask
        TaskAborter
    end

    subgraph HandleClientMessage
        ConnectionInit -->|Initialize Connection| HandleConnectionInit
        Subscribe -->|Start Subscription| HandleSubscribe
        Complete -->|Stop Poller| StopPoller
        Ping -->|Respond with Pong| SendPong
        Pong -->|No Action| NoAction
    end

    HandleSubscribe -->|Check Init State| CheckInitState
    CheckInitState -->|Not Initialized| SendUnauthorized
    CheckInitState -->|Initialized| StartPoller

    StartPoller -->|Poll Subscription| PollSubscription
    PollSubscription -->|Execute GraphQL Request| ExecuteGraphQLRequest

    ExecuteGraphQLRequest -->|Mutation Plan| HandleMutationPlan
    ExecuteGraphQLRequest -->|Query Plan| HandleQueryPlan
    ExecuteGraphQLRequest -->|Subscription Plan| HandleSubscriptionPlan

    HandleMutationPlan -->|Send Result| SendSingleResult
    HandleQueryPlan -->|Send Result| SendSingleResult

    HandleSubscriptionPlan -->|Poll Data| FetchFromDataConnector
    FetchFromDataConnector -->|Success| ProcessSubscriptionResponse
    FetchFromDataConnector -->|Error| SendGraphQLError

    ProcessSubscriptionResponse -->|Match Hash| CheckResponseHash
    CheckResponseHash -->|Hash Matches| DoNothing
    CheckResponseHash -->|New Response| SendGraphQLResponse

    SendGraphQLResponse -->|No Errors| SendGraphQLOk
    SendGraphQLResponse -->|Has Errors| SendGraphQLError

    SendGraphQLOk -->|Polls Again| FetchFromDataConnector
    DoNothing -->|Polls Again| FetchFromDataConnector
```
