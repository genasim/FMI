import { Recipe, RecipeDTO } from "../models/Recipe";
import API, { Tables } from "../shared/api-client/ApiClient";

const createRecipe = async (entity: RecipeDTO) => {
  try {
    const recipe = await API.create<Recipe>(Tables.RECIPES, entity);
    return recipe;
  } catch (error) {
    console.error(error);
  }
};

export default createRecipe;
