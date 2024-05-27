import { FC } from "react";
import { useLoaderData, useNavigate } from "react-router-dom";
import { Recipe } from "../../models/Recipe";
import { Badge, Button, Col, Image, Row } from "react-bootstrap";
import { PiForkKnifeBold } from "react-icons/pi";

const RecipeDetails: FC = () => {
  const recipe = useLoaderData() as Recipe;
  const navigate = useNavigate();

  return (
    <section>
      <Button
        className="ms-5 mb-4"
        variant="outline-secondary"
        onClick={() => navigate(-1)}
      >
        Back
      </Button>
      <Row>
        <Col md="5">
          <Image
            style={{ height: "30rem", width: "30rem", objectFit: "cover" }}
            className="rounded-3"
            fluid
            src={recipe.imageUrl}
          />
        </Col>
        <Col md="1" />
        <Col md="6">
          <div className="mb-4">
            <h2>{recipe.name}</h2>
            <h6>{recipe.shortDescription}</h6>
            <div className=" mb-3 d-flex flex-row flex-wrap">
              {recipe.tags.map((tag) => (
                <Badge className="ms-1" key={tag} bg="success">
                  {tag}
                </Badge>
              ))}
            </div>
            <small>
              Shared on {recipe.shareDatetime.toUTCString()} <br />
              By {recipe.userName}
            </small>
          </div>
          <p>
            <i className="bi bi-clock"></i> Cooking time: {recipe.cookingTime}{" "}
            mins
          </p>
          <div>
            <PiForkKnifeBold /> Ingredients:
          </div>
          <ul>
            {recipe.ingredients.map((entry) => (
              <li key={entry}>{entry}</li>
            ))}
          </ul>
        </Col>
      </Row>
      <div className="mt-4">
        <h4>Description:</h4>
        <p>{recipe.description}</p>
        {sessionStorage.getItem("token") === recipe.userId && (
          <Button
            variant="info"
            onClick={() => navigate(`../edit/${recipe.id}`)}
          >
            Edit Recipe
          </Button>
        )}
      </div>
    </section>
  );
};

export default RecipeDetails;
