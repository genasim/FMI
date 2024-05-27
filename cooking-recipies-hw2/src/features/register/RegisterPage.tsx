import { FC, useContext } from "react";
import { Button } from "react-bootstrap";
import { useNavigate } from "react-router-dom";
import { UserDTO } from "../../models/User";
import createUser from "../../services/create-user";
import { LogoutContext } from "../../shared/layout/Layout";
import RegisterForm from "./RegisterForm";

const RegisterPage: FC = () => {
  const { setLogout } = useContext(LogoutContext);
  const navigate = useNavigate();

  const handleRegister = async (entity: UserDTO) => {
    const user = await createUser(entity);
    if (!user) return

    sessionStorage.setItem("token", user.id);
    sessionStorage.setItem("user-name", user.name)
    setLogout(true);
    navigate("/");
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
        <RegisterForm onRegister={handleRegister} />
      </div>
    </section>
  );
};

export default RegisterPage;
