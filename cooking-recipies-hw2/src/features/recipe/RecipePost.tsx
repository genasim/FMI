import { FC } from "react";
import { Button } from "react-bootstrap";
import { useNavigate } from "react-router-dom";
import RecipeForm from "./RecipeForm";
import { RecipeDTO } from "../../models/Recipe";
import createRecipe from "../../services/create-recipe";

const RecipePost: FC = () => {
  const navigate = useNavigate();
  
    const handleRecipePost = async (recipe: RecipeDTO) => {
        await createRecipe(recipe)
        navigate("..")
    }

  return (
    <section>
      <Button
        className="ms-5 mb-4"
        variant="outline-secondary"
        onClick={() => navigate(-1)}
      >
        Back
      </Button>
      <RecipeForm onPost={handleRecipePost} />
    </section>
  );
};

export default RecipePost;
