import { FC } from "react";
import { Button } from "react-bootstrap";
import { useLoaderData, useNavigate } from "react-router-dom";
import { Recipe } from "../../models/Recipe";
import updateRecipe from "../../services/update-recipe";
import EditRecipeForm from "./EditRecipeForm";

const EditRecipe: FC = () => {
  const recipe = useLoaderData() as Recipe;
  const navigate = useNavigate();

  const handleRecipeUpdate = async (updated: Recipe) => {
    await updateRecipe(updated);
    navigate("..");
  };

  return (
    <section>
      <Button
        variant="outline-secondary"
        className="ms-5"
        onClick={() => navigate("..")}
      >
        Back
      </Button>
      <div
        className="modal show"
        style={{ display: "block", position: "initial" }}
      >
        <EditRecipeForm recipe={recipe} onPost={handleRecipeUpdate} />
      </div>
    </section>
  );
};

export default EditRecipe;
