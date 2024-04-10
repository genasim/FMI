import React from "react";
import "./App.css";
import TodoInput from "./TodoInput";
import TodoList from "./TodoList";
import { Todo, TodoCreateDto } from "./todo-model";
import TodoRepository from "./todo-repository";
import API from "./todo-api-client";

interface AppState {
  todos: Todo[];
  errors: string;
}

class App extends React.Component<{}, AppState> {
  state: AppState = {
    todos: [],
    errors: "",
  };

  async componentDidMount() {
    try {
      const todos = await API.findAll(Todo);
      this.setState({ todos });
      this.clearErrors()
    } catch (error) {
      this.showError(error as Error);
    }
  }

  createTodo = async (todo: TodoCreateDto) => {
    const created = await TodoRepository.create(todo);
    this.setState((state) => ({ todos: [...state.todos, created] }));
  };

  showError = (error: Error) => {
    this.setState({ errors: error.message });
  };

  clearErrors = () => {
    this.setState({ errors: "" });
  };

  render() {
    return (
      <div className="container d-flex flex-column justify-content-between text-black bg-light">
        <TodoInput onCreateTodo={this.createTodo} onError={this.showError} />
        {this.state.errors && <div className="errors">Error: {this.state.errors}</div>}
        <TodoList todos={this.state.todos} />
      </div>
    );
  }
}

export default App;
