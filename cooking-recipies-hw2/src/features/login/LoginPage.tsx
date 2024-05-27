import { FC, useContext } from "react";
import LoginForm from "./LoginForm";
import API, { Tables } from "../../shared/api-client/ApiClient";
import { User } from "../../models/User";
import { useNavigate } from "react-router-dom";
import { LogoutContext } from "../../shared/layout/Layout";
import { Button } from "react-bootstrap";

const LoginPage: FC = () => {
  const { setLogout } = useContext(LogoutContext);

  const navigate = useNavigate();
  const handleLogin = async ({
    username,
    password,
  }: {
    username: string;
    password: string;
  }) => {
    try {
      const users = await API.findAll<User>(Tables.USERS);
      const curr = users.filter(
        (user) => user.username === username && user.password === password
      );

      if (curr.length === 1) {
        sessionStorage.setItem("token", curr[0].id);
        sessionStorage.setItem("user-name", curr[0].name)
        setLogout(true);
        navigate("/");
      }
    } catch (error) {
      console.error(error);
    }
  };
  return (
    <section>
      <Button
        variant="outline-secondary"
        className="ms-5"
        onClick={() => navigate("/")}
      >
        Back
      </Button>
      <div
        className="modal show"
        style={{ display: "block", position: "initial" }}
      >
        <LoginForm onLogin={handleLogin} />
      </div>
    </section>
  );
};

export default LoginPage;
