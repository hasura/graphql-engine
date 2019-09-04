export type Todo = {
  id: string,
  title: string,
  is_completed?: boolean,
  is_public?: boolean,
  user?: {name: string}
}

export interface TodoPrivateListState {
  filter: string, clearInProgress: boolean, todos: Array<Todo>
}

export interface TodoPublicListState {
  filter?: string, clearInProgress?: boolean, todos: Array<Todo>, olderTodosAvailable: boolean, newTodosCount: number
}

