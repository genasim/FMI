import { Recipe } from "../models/Recipe";
import API, { Tables } from "../shared/api-client/ApiClient";

const updateRecipe = async (user: Recipe) => {
  try {
    const updated = await API.update<Recipe>(Tables.RECIPES, user);
    return updated;
  } catch (error) {
    console.error(error);
  }
};

export default updateRecipe;
