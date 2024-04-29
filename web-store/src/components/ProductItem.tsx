import { FC } from "react";
import { ProductCategory, Product } from "../models/product";
import EditProduct from "./EditProduct";

interface ProductItemProps {
  product: Product;
  onDelete: (product: Product) => void;
  onEdit: (product: Product) => void;
}

const ProductItem: FC<ProductItemProps> = ({ product, onDelete, onEdit }) => {
  return (
    <div className="card" style={{ width: "18rem" }}>
      <img src={product.imageUrl} className="card-img-top" alt="product" />
      <div className="card-body">
        <h5 className="card-title">{product.name}</h5>
        <p className="card-text">{product.details}</p>
        <p className="card-text">€{product.price}</p>
        <div className="d-flex justify-content-between">
          <button onClick={() => onDelete(product)} className="btn btn-danger">
            DEL
          </button>
          <EditProduct onUpdate={onEdit} product={product} />
          <span className="badge bg-info ">
            {ProductCategory[product.category]}
          </span>
        </div>
      </div>
    </div>
  );
};

export default ProductItem;
