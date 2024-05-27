import { yupResolver } from "@hookform/resolvers/yup";
import { FC } from "react";
import { Button, Col, Form, Modal, Row } from "react-bootstrap";
import { SubmitHandler, useForm } from "react-hook-form";
import * as yup from "yup";
import { RecipeDTO } from "../../models/Recipe";

interface RecipeFormProps {
  onPost: (user: RecipeDTO) => void;
}

interface FormData {
  name: string;
  shortDescription: string;
  description: string;
  cookingTime: number;
  ingredients: string;
  imageUrl: string;
  tags: string;
}

const formSchema = yup.object({
  name: yup
    .string()
    .required("Name is required")
    .max(80, "Name must be 80 characters or less"),
  shortDescription: yup
    .string()
    .required("Short description is required")
    .max(256, "Short description must be 256 characters or less"),
  description: yup
    .string()
    .required("Description is required")
    .max(2048, "Description must be 2048 characters or less"),
  ingredients: yup.string().required("Ingredients are required"),
  cookingTime: yup
    .number()
    .required("Cooking time is required")
    .moreThan(0, "Cooking time cannot be 0"),
  imageUrl: yup
    .string()
    .required("Image URL is required")
    .url("Invalid URL format"),
  tags: yup.string().required("Tags are required"),
});

const RecipeForm: FC<RecipeFormProps> = ({ onPost }) => {
  const { handleSubmit, register, formState } = useForm<FormData>({
    defaultValues: {
      name: "",
      shortDescription: "",
      description: "",
      cookingTime: 0,
      ingredients: "",
      imageUrl: "",
      tags: "",
    },
    resolver: yupResolver(formSchema),
    mode: "onBlur",
  });

  const onSubmit: SubmitHandler<FormData> = (data) => {
    const recipeDto: RecipeDTO = {
      name: data.name,
      cookingTime: data.cookingTime,
      description: data.description,
      shortDescription: data.shortDescription,
      imageUrl: data.imageUrl,
      ingredients: data.ingredients.split(","),
      tags: data.tags.split(","),
      userId: sessionStorage.getItem("token") ?? "",
      userName: sessionStorage.getItem("user-name") ?? "",
      shareDatetime: new Date(),
      lastModDatetime: new Date(),
    };

    onPost(recipeDto);
  };

  return (
    <div
      className="modal show"
      style={{ display: "block", position: "initial" }}
    >
      <Form onSubmit={handleSubmit(onSubmit)}>
        <Modal.Dialog size="lg">
          <Modal.Header>
            <Modal.Title>Post Recipe</Modal.Title>
          </Modal.Header>

          <Modal.Body>
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
                  <Form.Label>Cooking Time</Form.Label>
                  <Form.Control type="number" {...register("cookingTime")} />
                  <Form.Control.Feedback className="d-block" type="invalid">
                    {formState.errors.cookingTime?.message}
                  </Form.Control.Feedback>
                </Form.Group>
              </Col>{" "}
            </Row>

            <Row>
              <Col md="6">
                <Form.Group className="mb-3">
                  <Form.Label>Tags</Form.Label>
                  <Form.Control
                    placeholder="comma-separated values"
                    {...register("tags")}
                  />
                  <Form.Control.Feedback className="d-block" type="invalid">
                    {formState.errors.tags?.message}
                  </Form.Control.Feedback>
                </Form.Group>
              </Col>
              <Col md="6">
                <Form.Group className="mb-3">
                  <Form.Label>Ingredients</Form.Label>
                  <Form.Control
                    placeholder="comma-separated values"
                    {...register("ingredients")}
                  />
                  <Form.Control.Feedback className="d-block" type="invalid">
                    {formState.errors.ingredients?.message}
                  </Form.Control.Feedback>
                </Form.Group>
              </Col>
            </Row>

            <Form.Group className="mb-3">
              <Form.Label>Image URL</Form.Label>
              <Form.Control {...register("imageUrl")} />
              <Form.Control.Feedback className="d-block" type="invalid">
                {formState.errors.imageUrl?.message}
              </Form.Control.Feedback>
            </Form.Group>

            <Form.Group className="mb-3">
              <Form.Label>Short Description</Form.Label>
              <Form.Control
                as="textarea"
                rows={2}
                {...register("shortDescription")}
              />
              <Form.Control.Feedback className="d-block" type="invalid">
                {formState.errors.shortDescription?.message}
              </Form.Control.Feedback>
            </Form.Group>

            <Form.Group className="mb-3">
              <Form.Label>Description</Form.Label>
              <Form.Control
                as="textarea"
                rows={3}
                {...register("description")}
              />
              <Form.Control.Feedback className="d-block" type="invalid">
                {formState.errors.description?.message}
              </Form.Control.Feedback>
            </Form.Group>
          </Modal.Body>

          <Modal.Footer>
            <Button type="submit" variant="primary">
              Post
            </Button>
          </Modal.Footer>
        </Modal.Dialog>
      </Form>
    </div>
  );
};

export default RecipeForm;
