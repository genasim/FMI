import { FC, useState } from "react";
import { Col, Form, Row } from "react-bootstrap";
import { Recipe } from "../../models/Recipe";
import API, { Tables } from "../../shared/api-client/ApiClient";
import useAsyncEffect from "../../shared/hooks/useAsyncEffect";
import RecipeList from "./RecipeList";

interface ListFilter {
  tags: string;
  user: string;
}

const HomePage: FC = () => {
  const [recipes, setRecipes] = useState<Recipe[]>([]);
  const [filter, setFilter] = useState<ListFilter>({ tags: "", user: "" });
  const [error, setError] = useState<Error>();

  useAsyncEffect(async () => {
    try {
      const recipes = await API.findAll<Recipe>(Tables.RECIPES);
      recipes.map(recipe => recipe.shareDatetime = new Date(recipe.shareDatetime))
      setRecipes(recipes);
    } catch (error) {
      console.error(error);
      setError(error as Error);
    }
  }, []);

  const handleFilterChange = (e: any) => {
    setFilter({
      ...filter,
      [e.target.name]: e.target.value,
    });
  };

  if (error) {
    return <h1>{error.message}</h1>;
  }

  return (
    <section>
      <Form className="my-5">
        <Row>
          <Col md="4">
            <Form.Label htmlFor="user-filter">User filter</Form.Label>
            <Form.Control
              id="user-filter"
              name="user"
              value={filter?.user}
              onChange={handleFilterChange}
            />
          </Col>
          <Col md="3" />
          <Col md="5">
            <Form.Label htmlFor="tags-filter">
              Tags filter (comma-separated)
            </Form.Label>
            <Form.Control
              id="tags-filter"
              name="tags"
              value={filter?.tags}
              onChange={handleFilterChange}
            />
          </Col>
        </Row>
      </Form>
      <RecipeList
        recipes={recipes}
        tags={filter?.tags === "" ? undefined : filter?.tags}
        user={filter?.user === "" ? undefined : filter?.user}
      />
    </section>
  );
};

export default HomePage;
