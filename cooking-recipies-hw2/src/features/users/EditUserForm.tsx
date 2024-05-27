import { yupResolver } from "@hookform/resolvers/yup";
import { FC } from "react";
import { Button, Col, Form, Modal, Row } from "react-bootstrap";
import { SubmitHandler, useForm } from "react-hook-form";
import * as yup from "yup";
import { AccountStatus } from "../../models/AccountStatus";
import { Role } from "../../models/Role";
import { Sex } from "../../models/Sex";
import { User } from "../../models/User";

interface EditUserProps {
  user: User;
  onEdit: (user: User) => void;
}

interface FormData {
  name: string;
  username: string;
  password: string;
  sex: Sex;
  role: Role;
  status: AccountStatus;
  imageUrl?: string;
  bio: string;
}

const formSchema = yup.object({
  username: yup
    .string()
    .required("Username is required")
    .max(15, "Username must be 15 characters or less"),
  password: yup
    .string()
    .required("Password is required")
    .min(8, "Password must be minimum 8 characters long")
    .matches(
      /^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$/,
      "Password must contain at least one letter, one number, and one special character"
    ),
  status: yup
    .mixed<AccountStatus>()
    .oneOf(
      [
        AccountStatus.Active,
        AccountStatus.Suspended,
        AccountStatus.Deactivated,
      ],
      "Invalid status value"
    )
    .required("Status is required"),
  bio: yup
    .string()
    .required("Bio is required")
    .max(512, "Bio must be 512 characters or less"),
  sex: yup
    .mixed<Sex>()
    .oneOf([Sex.Male, Sex.Female], "Invalid sex value")
    .required("Sex is required"),
  imageUrl: yup
    .string()
    .url("Invalid URL format"),
  name: yup.string().required("Name is required"),
  role: yup
    .mixed<Role>()
    .oneOf([Role.User, Role.Admin], "Invalid role value")
    .required("Role is required"),
});

const EditUserForm: FC<EditUserProps> = ({ user, onEdit }) => {
  const { handleSubmit, register, formState } = useForm<FormData>({
    defaultValues: {
      username: user.username,
      password: user.password,
      bio: user.bio,
      sex: user.sex,
      status: user.status,
      imageUrl: user.imageUrl,
      name: user.name,
      role: user.role,
    },
    resolver: yupResolver(formSchema),
    mode: "onBlur",
  });

  const onSubmit: SubmitHandler<FormData> = (data) => {
    const updated: User = {
      id: user.id,
      username: data.username,
      password: data.password,
      bio: data.bio,
      sex: data.sex,
      imageUrl: data.imageUrl,
      name: data.name,
      role: data.role,
      status: data.status,
      registerDate: user.registerDate,
      lastModDatetime: new Date(),
    };

    onEdit(updated);
  };

  return (
    <Form onSubmit={handleSubmit(onSubmit)}>
      <Modal.Dialog size="lg">
        <Modal.Header>
          <Modal.Title>Edit user</Modal.Title>
        </Modal.Header>

        <Modal.Body>
          <Row>
            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Username</Form.Label>
                <Form.Control {...register("username")} />
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.username?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>

            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Password</Form.Label>
                <Form.Control type="password" {...register("password")} />
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.password?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>
          </Row>

          <Row>
            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Name</Form.Label>
                <Form.Control {...register("name")} />
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.name?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>

            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Account status</Form.Label>
                <Form.Select {...register("status")}>
                  <option value=""></option>
                  <option value={AccountStatus.Active}>Active</option>
                  <option value={AccountStatus.Suspended}>Suspended</option>
                  <option value={AccountStatus.Deactivated}>Deactivated</option>
                </Form.Select>
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.status?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>
          </Row>

          <Row>
            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Role</Form.Label>
                <Form.Select {...register("role")}>
                  <option value=""></option>
                  <option value={Role.User}>User</option>
                  <option value={Role.Admin}>Admin</option>
                </Form.Select>
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.role?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>

            <Col md="6">
              <Form.Group className="mb-3">
                <Form.Label>Sex</Form.Label>
                <Form.Select {...register("sex")}>
                  <option value=""></option>
                  <option value={Sex.Male}>Male</option>
                  <option value={Sex.Female}>Female</option>
                </Form.Select>
                <Form.Control.Feedback className="d-block" type="invalid">
                  {formState.errors.sex?.message}
                </Form.Control.Feedback>
              </Form.Group>
            </Col>
          </Row>

          <Row>
            <Form.Group className="mb-3">
              <Form.Label>Short biography</Form.Label>
              <Form.Control as="textarea" rows={3} {...register("bio")} />
              <Form.Control.Feedback className="d-block" type="invalid">
                {formState.errors.bio?.message}
              </Form.Control.Feedback>
            </Form.Group>
          </Row>

          <Row>
            <Form.Group className="mb-3">
              <Form.Label>Image URL</Form.Label>
              <Form.Control {...register("imageUrl")} />
              <Form.Control.Feedback className="d-block" type="invalid">
                {formState.errors.imageUrl?.message}
              </Form.Control.Feedback>
            </Form.Group>
          </Row>
        </Modal.Body>

        <Modal.Footer>
          <Button type="submit" variant="primary">
            Edit user
          </Button>
        </Modal.Footer>
      </Modal.Dialog>
    </Form>
  );
};

export default EditUserForm;
