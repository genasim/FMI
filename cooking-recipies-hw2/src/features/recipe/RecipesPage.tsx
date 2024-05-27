import { FC, useState } from "react";
import { Table, Image, Button } from "react-bootstrap";
import { useLoaderData, useNavigate } from "react-router-dom";
import { Recipe } from "../../models/Recipe";
import deleteRecipe from "../../services/delete-recipe";

const RecipesPage: FC = () => {
  const data = useLoaderData() as Recipe[];
  const [recipes, setRecipes] = useState<Recipe[]>(data);

  const navigate = useNavigate();

  const handleRecipeDelte = async (recipe: Recipe) => {
    const deleted = await deleteRecipe(recipe);
    setRecipes(recipes.filter((recipe) => recipe.id !== deleted?.id));
  };

  const handleAddRecipePost = () => {
    if (sessionStorage.getItem("token")) {
        navigate("post")
    }
    else {
        navigate("/login")
    }
  }

  return (
    <section>
      <Button
        className="float-end me-5 mb-4"
        variant="outline-success"
        onClick={handleAddRecipePost}
      >
        + Add recipe
      </Button>
      <Table striped>
        <thead>
          <tr>
            <th></th>
            <th>Name</th>
            <th>Cooking Time</th>
            <th>Shared by</th>
            <th>Posted on</th>
            <th>Last Changed</th>
            <th className="text-end pe-4">Actions</th>
          </tr>
        </thead>
        <tbody>
          {recipes.map((recipe) => (
            <tr key={recipe.id} className="align-middle">
              <td>
                {
                  <Image
                    style={{
                      height: "3rem",
                      width: "3rem",
                      objectFit: "cover",
                    }}
                    roundedCircle
                    src={recipe.imageUrl}
                  />
                }
              </td>
              <td>{recipe.name}</td>
              <td>{recipe.cookingTime} mins</td>
              <td>{recipe.userName}</td>
              <td>{recipe.shareDatetime.toString()}</td>
              <td>{recipe.lastModDatetime.toString()}</td>
              <td className="text-end">
                <Button variant="info" onClick={() => navigate(`${recipe.id}`)}>
                  <i className="bi bi-eye"></i>
                </Button>
                <Button
                  className="ms-2"
                  variant="danger"
                  onClick={() => handleRecipeDelte(recipe)}
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

export default RecipesPage;
