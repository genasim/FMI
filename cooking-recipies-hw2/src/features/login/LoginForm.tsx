import { yupResolver } from "@hookform/resolvers/yup";
import { FC } from "react";
import { Button, Form, Modal } from "react-bootstrap";
import { SubmitHandler, useForm } from "react-hook-form";
import { NavLink } from "react-router-dom";
import * as yup from "yup";
interface LoginFormProps {
  onLogin: (user: LoginFields) => void;
}

interface LoginFields {
  username: string;
  password: string;
}

const formSchema = yup.object({
  username: yup.string().required("Username is required"),
  password: yup
    .string()
    .required("Password is required")
    .min(8, "Password must be minimum 8 characters long")
    .matches(
      /^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$/,
      "Password must contain at least one letter, one number, and one special character"
    ),
});

const LoginForm: FC<LoginFormProps> = ({ onLogin }) => {
  const { handleSubmit, register, formState } = useForm<LoginFields>({
    defaultValues: { username: "", password: "" },
    resolver: yupResolver(formSchema),
    mode: "onBlur",
  });

  const onSubmit: SubmitHandler<LoginFields> = (data) => {
    onLogin(data);
  };

  return (
    <Form onSubmit={handleSubmit(onSubmit)}>
      <Modal.Dialog>
        <Modal.Header>
          <Modal.Title>Login</Modal.Title>
        </Modal.Header>

        <Modal.Body>
          <Form.Group className="mb-3">
            <Form.Label>Username</Form.Label>
            <Form.Control {...register("username")} />
            <Form.Control.Feedback className="d-block" type="invalid">
              {formState.errors.username?.message}
            </Form.Control.Feedback>
          </Form.Group>
          <Form.Group className="mb-3">
            <Form.Label>Password</Form.Label>
            <Form.Control type="password" {...register("password")} />
            <Form.Control.Feedback className="d-block" type="invalid">
              {formState.errors.password?.message}
            </Form.Control.Feedback>
          </Form.Group>
          
          <NavLink to="/register">Don't have an account? Register here</NavLink>
        </Modal.Body>

        <Modal.Footer>
          <Button type="submit" variant="primary">
            Login
          </Button>
        </Modal.Footer>
      </Modal.Dialog>
    </Form>
  );
};

export default LoginForm;
