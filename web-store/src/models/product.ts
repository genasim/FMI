import { IdType } from "../shared/shared-types";

export type Tag = string;

export enum ProductCategory {
  Computers = 0,
  Phones,
  Accessories,
  Software,
}

export enum ProductOrder {
  Ascending = 1,
  None = 0,
  Descending = -1,
}

export class ProductDTO {
  constructor(
    public name: string,
    public details: string,
    public price: number,
    public category: ProductCategory,
    public imageUrl: string,
    public tags?: Tag[]
  ) {}
}

export class Product extends ProductDTO {
  static className = "product";
  public id: IdType = 0;
}
