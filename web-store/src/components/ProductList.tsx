import { FC } from "react";
import { Product, ProductCategory, ProductOrder } from "../models/product";
import ProductItem from "./ProductItem";
import { Optional } from "../shared/shared-types";

interface ProductListProps {
  products: Product[];
  filter: Optional<ProductCategory>;
  order: Optional<ProductOrder>
  onDelete: (product: Product) => void;
  onEdit: (product: Product) => void;
}

const ProductList: FC<ProductListProps> = ({
  products,
  filter,
  order,
  onDelete,
  onEdit,
}) => {
  return (
    <div className="d-flex flex-row gap-3 flex-wrap justify-content-center">
      {products
        .filter((product) => {
          if (Number.isNaN(filter) || filter === undefined) {
            return product;
          }
          return product.category === filter;
        })
        .sort((a, b) => (a.price - b.price) * (order as number))
        .map((product) => (
          <ProductItem
            key={product.id}
            onEdit={onEdit}
            onDelete={onDelete}
            product={product}
          />
        ))}
    </div>
  );
};

export default ProductList;
