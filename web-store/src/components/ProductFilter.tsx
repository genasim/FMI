import { FC } from "react";
import { ProductCategory, ProductOrder } from "../models/product";
import { Optional } from "../shared/shared-types";

interface ProductFilterProps {
  category: Optional<ProductCategory>;
  onChangeCategory: (filter: Optional<ProductCategory>) => void;
  order: Optional<ProductOrder>;
  onChangeOrder: (order: ProductOrder) => void;
}

const ProductFilter: FC<ProductFilterProps> = ({
  category,
  onChangeCategory,
  onChangeOrder,
  order,
}) => {
  return (
    <div className="my-5 d-flex">
      <h4>Your current filter: </h4>
      <select
        value={category}
        onChange={(e) => onChangeCategory(parseInt(e.target.value))}
        className="mx-2"
      >
        <option value={undefined}></option>
        <option value={ProductCategory.Accessories}>Accessories</option>
        <option value={ProductCategory.Computers}>Computers</option>
        <option value={ProductCategory.Phones}>Phones</option>
        <option value={ProductCategory.Software}>Software</option>
      </select>
      <select
        value={order}
        onChange={(e) => onChangeOrder(parseInt(e.target.value))}
        className="mx-2"
      >
        <option value={ProductOrder.None}>None</option>
        <option value={ProductOrder.Ascending}>Ascending</option>
        <option value={ProductOrder.Descending}>Descending</option>
      </select>
    </div>
  );
};

export default ProductFilter;
