import { LoaderFunction } from "react-router-dom";
import { Recipe } from "../models/Recipe";
import API, { Tables } from "../shared/api-client/ApiClient";

const fetchAllRecipes: LoaderFunction = async () => {
  try {
    const recipes = await API.findAll<Recipe>(Tables.RECIPES);
    recipes.map((recipe) => ({
      ...recipe,
      registerDate: recipe.shareDatetime as Date,
      lastModDatetime: recipe.lastModDatetime as Date,
    }));
    return recipes;
  } catch (error) {
    console.error(error);
    return [];
  }
};

export default fetchAllRecipes;
