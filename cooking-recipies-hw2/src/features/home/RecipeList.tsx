import { FC, useEffect, useState } from "react";
import { Recipe } from "../../models/Recipe";
import RecipeCard from "./RecipeCard";

interface RecipeListProps {
  recipes: Recipe[];
  tags?: string;
  user?: string;
}

const RecipeList: FC<RecipeListProps> = ({ recipes, tags, user }) => {
  const [tagValues, setTagValues] = useState<string[]>();
  useEffect(() => {
    setTagValues(tags?.trim().split(","));
  }, [tags]);

  return (
    <div className="d-flex justify-content-center flex-column flex-wrap gap-5">
      {recipes
        .filter((recipe) =>
          tagValues === undefined
            ? true
            : tagValues.reduce(
                (prev, curr) =>
                  prev ||
                  recipe.tags
                    .map((tag) => tag.toLowerCase())
                    .includes(curr.toLowerCase()),
                false
              )
        )
        .filter((recipe) =>
          user === undefined
            ? true
            : recipe.userName?.toLowerCase().startsWith(user)
        )
        .sort((a, b) => b.shareDatetime.getTime() - a.shareDatetime.getTime())
        .slice(0, 9)
        .map((recipe) => (
          <RecipeCard key={recipe.id} recipe={recipe} />
        ))}
    </div>
  );
};

export default RecipeList;
