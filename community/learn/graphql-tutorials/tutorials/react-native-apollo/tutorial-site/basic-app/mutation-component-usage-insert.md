# Run the mutation and update the UI

In this React Native app, the todos are entered by the `Textbox` component in `src/screens/components/Textbox.js` based on the prop `isPublic` and other `session` props. Currently, it is a dysfunctional textbox. Let us go ahead and integrate this component with the GraphQL backend using the `Mutation` component.


1. Write the mutation and wrap it `gql`.

    ```js
    import gql from 'graphql-tag';

    const insertTodo = gql`
      mutation ($text: String!, $userId: String!, $isPublic: Boolean){
        insert_todos (
          objects: [{
            text: $text,
            user_id: $userId,
            is_public: $isPublic
          }]
        ){
          returning {
            id
            text
            is_completed
            created_at
            updated_at
            is_public
            user {
              name
            }
          }
        }
      }
    `;
    ```

    In this mutation, we are inserting a todo with variables `text`, `user_id`, and `is_public`  so that we can reuse this textbox for public and private `todos`.

2. Import the `<Mutation>` component:

    ```js
    import { Mutation } from 'react-apollo';
    ```

3. Render the textbox using a `Mutation` component. You can do that by modifying the render method to use the mutation from the `Mutation` component. Call the `insert_todo` mutation function when the button is pressed.


    ```js
    render() {
      const { text } = this.state;
      const { userId, isPublic } = this.props;
      return (
        <Mutation
          mutation={insertTodo}
          variables={{ text, userId, isPublic }}
          update={(cache, {data: {insert_todos}}) => {
            const data = cache.readQuery({
              query: fetchTodos,
              variables: { isPublic }
            });
            const newTodo = insert_todos.returning[0];
            const newData = {
              todos: [ newTodo, ...data.todos]
            }
            cache.writeQuery({
              query: fetchTodos,
              variables: { isPublic },
              data: newData
            });
          }}
        >
          {
            (insertTodo, { loading, error}) => {
              const submit = () => {
                if (text === '') {
                  return;
                }
                this.setState({
                  text: ''
                });
                insertTodo();
              }
              if (error) {
                return <Text> Error </Text>;
              }
              return (
                <View style={styles.inputContainer}>
                  <View style={styles.textboxContainer}>
                    <TextInput
                      style={styles.textbox}
                      editable = {true}
                      onChangeText = {this._handleTextChange}
                      value = {text}
                    />
                  </View>
                  <View style={styles.buttonContainer}>
                    <TouchableOpacity style={styles.button} onPress={submit} disabled={text === ''}>
                      <Text style={styles.buttonText}> Add </Text>
                    </TouchableOpacity>
                  </View>
                </View>
              );
            }
          }
        </Mutation>
      );
    }
    ```

4. We are also updating the Apollo Cache so that the UI updates instantly. In the update prop:

  - We read the existing data:
    
    ```js
    const data = cache.readQuery({
      query: fetchTodos,
      variables: { isPublic }
    });
    ``` 

  - We create a new data object by appending the new todo with the existing todos

    ```js
    const newTodo = insert_todos.returning[0];
    const newData = {
      todos: [ newTodo, ...data.todos]
    }
    ```

  - We rewrite the cache with the new data

    ```js
    cache.writeQuery({
      query: fetchTodos,
      variables: { isPublic },
      data: newData
    });
    ```

## Wrapping up

If done correctly, your `Textbox.js` should render a textbox and a button that is able to insert data into the `todos` table using `insert_todos` mutation and update the UI when the mutation is complete.

In the next section, we will enable the functionality of deleting and updating todos.
