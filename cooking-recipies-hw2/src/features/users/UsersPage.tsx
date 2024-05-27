import { FC, useState } from "react";
import { Button, Image, Table } from "react-bootstrap";
import { useLoaderData, useNavigate } from "react-router-dom";
import { Sex } from "../../models/Sex";
import { User } from "../../models/User";
import deleteUserId from "../../services/delete-user-id";

const stockFemaleAvatar =
  "https://as1.ftcdn.net/v2/jpg/01/68/80/20/1000_F_168802088_1msBk8PpBRCCVo012WJTpWG90KHvoMWf.jpg";
const stockMaleAvatar =
  "https://as1.ftcdn.net/v2/jpg/01/68/80/20/500_F_168802075_Il6LeUG0NCK4JOELmkC7Ki81g0CiLpxU.jpg";

const UsersPage: FC = () => {
  const data = useLoaderData() as User[];
  const [users, setUsers] = useState<User[]>(data);

  const navigate = useNavigate();

  const handleDeleteUser = async (user: User) => {
    const deleted = await deleteUserId(user.id ?? "");
    setUsers(users.filter((user) => user.id !== deleted?.id));
  };

  return (
    <section>
      <Table striped>
        <thead>
          <tr>
            <th></th>
            <th>Username</th>
            <th>Name</th>
            <th>status</th>
            <th>Register Date</th>
            <th>Role</th>
            <th className="text-end pe-4">Actions</th>
          </tr>
        </thead>
        <tbody>
          {users.map((user) => (
            <tr key={user.id} className="align-middle">
              <td>
                {
                  <Image
                    style={{
                      height: "3rem",
                      width: "3rem",
                      objectFit: "cover",
                    }}
                    roundedCircle
                    src={
                      user.imageUrl === undefined || user.imageUrl === ""
                        ? user.sex === Sex.Female
                          ? stockFemaleAvatar
                          : stockMaleAvatar
                        : user.imageUrl
                    }
                  />
                }
              </td>
              <td>{user.username}</td>
              <td>{user.name}</td>
              <td>{user.status}</td>
              <td>{user.registerDate.toString()}</td>
              <td>{user.role}</td>
              <td className="text-end">
                <Button variant="info" onClick={() => navigate(`${user.id}`)}>
                  <i className="bi bi-pencil-square"></i>
                </Button>
                <Button
                  className="ms-2"
                  variant="danger"
                  onClick={() => handleDeleteUser(user)}
                >
                  <i className="bi bi-trash3-fill"></i>
                </Button>
              </td>
            </tr>
          ))}
        </tbody>
      </Table>
    </section>
  );
};

export default UsersPage;
