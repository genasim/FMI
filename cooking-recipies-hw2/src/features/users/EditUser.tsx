import { FC } from "react";
import { Button } from "react-bootstrap";
import { useLoaderData, useNavigate } from "react-router-dom";
import { User } from "../../models/User";
import updateUser from "../../services/update-user";
import EditUserForm from "./EditUserForm";

const EditUser: FC = () => {
  const user = useLoaderData() as User;
  const navigate = useNavigate();

  const handleUserUpdate = async (user: User) => {
    await updateUser(user);
    navigate("..");
  };

  return (
    <section>
      <Button
        variant="outline-secondary"
        className="ms-5"
        onClick={() => navigate(-1)}
      >
        Back
      </Button>
      <div
        className="modal show"
        style={{ display: "block", position: "initial" }}
      >
        <EditUserForm user={user} onEdit={handleUserUpdate} />
      </div>
    </section>
  );
};

export default EditUser;
