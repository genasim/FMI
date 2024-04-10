import { FormEvent, useState } from "react";
import { TodoCreateDto, TodoStatus } from "./todo-model";

type Props = {
  onCreateTodo: (todo: TodoCreateDto) => void;
  onError: (error: Error) => void;
};

const TodoInput = ({ onCreateTodo, onError }: Props) => {
  const [text, setText] = useState<string>("");
  const [status, setStatus] = useState<TodoStatus>(TodoStatus.Active);

  const submitTodo = (e: FormEvent) => {
    e.preventDefault();

    if (text.trim().length === 0) {
      onError(new Error("All fields are required"));
      return;
    }

    const todo = new TodoCreateDto(text, status);
    onCreateTodo(todo);
    resetTodo();
  };

  const resetTodo = (e?: FormEvent) => {
    e?.preventDefault();
    setText("");
    setStatus(TodoStatus.Active);
  };

  return (
    <div>
      <form onSubmit={submitTodo} onReset={resetTodo}>
        <input
          type="text"
          value={text}
          onChange={(event) => setText(event.target.value)}
        />
        <select
          value={status}
          onChange={(event) => setStatus(parseInt(event.target.value))}
        >
          <option value={TodoStatus.Active}>Active</option>
          <option value={TodoStatus.Canceled}>Canceled</option>
          <option value={TodoStatus.Completed}>Completed</option>
        </select>
        <button type="submit">Submit</button>
        <button type="reset">Reset</button>
      </form>
    </div>
  );
};

export default TodoInput;
