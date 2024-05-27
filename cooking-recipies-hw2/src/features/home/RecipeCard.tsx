import { FC } from "react";
import { Badge, Button, Card, Col, Row } from "react-bootstrap";
import { Recipe } from "../../models/Recipe";
import { useNavigate } from "react-router-dom";

interface RecipeCardProps {
  recipe: Recipe;
}

const RecipeCard: FC<RecipeCardProps> = ({ recipe }) => {
  const navigate = useNavigate()

  return (
    <Card>
      <Row>
        <Col md="3">
          <Card.Img
            className="rounded-end-0"
            style={{ height: "10rem", objectFit: "cover" }}
            src={recipe.imageUrl}
          />
        </Col>
        <Col md="9">
          <Card.Body className="h-75">
            <div className="d-flex justify-content-between">
              <Card.Title>{recipe.name}</Card.Title>
              <div>
                {recipe.tags.map((tag) => (
                  <Badge className="ms-1" key={tag} bg="success">
                    {tag}
                  </Badge>
                ))}
              </div>
            </div>
            <div className="d-flex h-100 flex-column">
              <Card.Text className="flex-grow-1">
                {recipe.shortDescription}
              </Card.Text>
              <Card.Text>
                <small className="float-start">
                  Shared on {recipe.shareDatetime.toUTCString()} by{" "}
                  {recipe.userName}
                </small>
                <Button className="float-end" variant="info" onClick={() => navigate(`/recipes/${recipe.id}`)}>
                  <i className="bi bi-eye"></i>
                </Button>
              </Card.Text>
            </div>
          </Card.Body>
        </Col>
      </Row>
    </Card>
  );
};

export default RecipeCard;
