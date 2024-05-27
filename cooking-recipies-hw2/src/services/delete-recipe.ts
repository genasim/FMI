import { Recipe } from "../models/Recipe";
import API, { Tables } from "../shared/api-client/ApiClient";

const deleteRecipe = async (recipe: Recipe) => {
  try {
    const deleted = await API.deleteById<Recipe>(Tables.RECIPES, recipe.id);
    return deleted;
  } catch (error) {
    console.error(error);
  }
};

export default deleteRecipe;
