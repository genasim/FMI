import { RouterProvider, createBrowserRouter } from "react-router-dom";
import "./App.css";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import Layout from "./shared/layout/Layout";
import ErrorBoundry from "./shared/layout/ErrorBoundry";
import NotFoundPage from "./shared/layout/404";
import HomePage from "./features/home/HomePage";
import LoginPage from "./features/login/LoginPage";
import RegisterPage from "./features/register/RegisterPage";
import UsersPage from "./features/users/UsersPage";
import fetchAllUsers from "./services/fetch-all-users";
import EditUser from "./features/users/EditUser";
import fetchUserId from "./services/fetch-user-id";
import RecipesPage from "./features/recipe/RecipesPage";
import fetchAllRecipes from "./services/fetch-all-recipes";
import fetchRecipeId from "./services/fetch-recipe-id";
import RecipeDetails from "./features/recipe/RecipeDetails";
import RecipePost from "./features/recipe/RecipePost";
import EditRecipe from "./features/recipe/EditRecipe";

const router = createBrowserRouter([
  {
    path: "/",
    element: <Layout />,
    errorElement: <ErrorBoundry />,
    children: [
      {
        index: true,
        element: <HomePage />,
      },
      {
        path: "/login",
        element: <LoginPage />,
      },
      {
        path: "/register",
        element: <RegisterPage />,
      },
      {
        path: "/users",
        children: [
          {
            index: true,
            element: <UsersPage />,
            loader: fetchAllUsers,
          },
          {
            path: ":userId",
            element: <EditUser />,
            loader: fetchUserId,
          },
        ],
      },
      {
        path: "/recipes",
        children: [
          {
            index: true,
            element: <RecipesPage />,
            loader: fetchAllRecipes,
          },
          {
            path: "post",
            element: <RecipePost />
          },
          {
            path: ":recipeId",
            element: <RecipeDetails />,
            loader: fetchRecipeId
          },
          {
            path: "edit/:recipeId",
            element: <EditRecipe />,
            loader: fetchRecipeId,
          },
        ],
      },
      {
        path: "*",
        element: <NotFoundPage />,
      },
    ],
  },
]);

const App = () => {
  return <RouterProvider router={router} />;
};

export default App;
