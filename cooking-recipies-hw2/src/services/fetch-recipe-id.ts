import { LoaderFunction } from "react-router-dom";
import API, { Tables } from "../shared/api-client/ApiClient";
import { Recipe } from "../models/Recipe";

const fetchRecipeId: LoaderFunction = async ({ params }) => {
  const { recipeId } = params;
  try {
    const recipe = await API.findById<Recipe>(Tables.RECIPES, recipeId ?? "");
    recipe.shareDatetime = new Date(recipe.shareDatetime);
    return recipe;
  } catch (error) {
    console.error(error);
  }
};

export default fetchRecipeId;
