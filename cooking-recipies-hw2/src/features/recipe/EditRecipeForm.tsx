import { yupResolver } from "@hookform/resolvers/yup";
import { FC } from "react";
import { Button, Col, Form, Modal, Row } from "react-bootstrap";
import { SubmitHandler, useForm } from "react-hook-form";
import * as yup from "yup";
import { Recipe } from "../../models/Recipe";

interface RecipeFormProps {
  recipe: Recipe;
  onPost: (user: Recipe) => void;
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

const EditRecipeForm: FC<RecipeFormProps> = ({ recipe, onPost }) => {
  const { handleSubmit, register, formState } = useForm<FormData>({
    defaultValues: {
      name: recipe.name,
      shortDescription: recipe.shortDescription,
      description: recipe.description,
      cookingTime: recipe.cookingTime,
      ingredients: recipe.ingredients.join(","),
      imageUrl: recipe.imageUrl,
      tags: recipe.tags.join(","),
    },
    resolver: yupResolver(formSchema),
    mode: "onBlur",
  });

  const onSubmit: SubmitHandler<FormData> = (data) => {
    const updated: Recipe = {
      ...recipe,
      name: data.name,
      cookingTime: data.cookingTime,
      description: data.description,
      shortDescription: data.shortDescription,
      imageUrl: data.imageUrl,
      ingredients: data.ingredients.split(","),
      tags: data.tags.split(","),
      lastModDatetime: new Date(),
    };

    onPost(updated);
  };

  return (
    <div
      className="modal show"
      style={{ display: "block", position: "initial" }}
    >
      <Form onSubmit={handleSubmit(onSubmit)}>
        <Modal.Dialog size="lg">
          <Modal.Header>
            <Modal.Title>Edit  Recipe</Modal.Title>
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
              Edit
            </Button>
          </Modal.Footer>
        </Modal.Dialog>
      </Form>
    </div>
  );
};

export default EditRecipeForm;
