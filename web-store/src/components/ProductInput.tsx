import { FC, FormEvent, useCallback, useState } from "react";
import { ProductCategory, ProductDTO } from "../models/product";

interface ProductInputProps {
  onCreate: (product: ProductDTO) => void;
  onError: (error: Error) => void;
}

const ProductInput: FC<ProductInputProps> = ({ onCreate, onError }) => {
  const [title, setTitle] = useState<string>("");
  const [details, setDetails] = useState<string>("");
  const [category, setCategory] = useState<ProductCategory>(
    ProductCategory.Accessories
  );
  const [price, setPrice] = useState<number>(0);
  const [imageUrl, setImageUrl] = useState<string>("");

  const handleSubmit = useCallback(
    (event: FormEvent) => {
      event.preventDefault();
      if (
        title.trim().length === 0 ||
        price === 0 ||
        details.trim().length === 0
      ) {
        onError(new Error("All fields are required"));
        return;
      }

      const product = new ProductDTO(title, details, price, category, imageUrl);

      onCreate(product);
      resetForm();
    },
    [title, price, details, category, imageUrl, onCreate, onError]
  );

  const resetForm = (e?: FormEvent) => {
    e?.preventDefault();
    setTitle("");
    setPrice(0);
    setDetails("");
    setCategory(ProductCategory.Accessories);
    setImageUrl("");
  };

  return (
    <form onSubmit={handleSubmit} onReset={resetForm} className="my-5">
      <div className="d-flex flex-row">
        <input
          onChange={(e) => setTitle(e.target.value)}
          value={title}
          type="text"
          className="flex-fill"
        />
        <input
          onChange={(e) => setPrice(parseInt(e.target.value))}
          value={price}
          type="number"
        />
        <select onChange={(e) => setCategory(parseInt(e.target.value))}>
          <option value={ProductCategory.Accessories}>Accessories</option>
          <option value={ProductCategory.Computers}>Computers</option>
          <option value={ProductCategory.Phones}>Phones</option>
          <option value={ProductCategory.Software}>Software</option>
        </select>
      </div>
      <input
        onChange={(e) => setDetails(e.target.value)}
        value={details}
        type="text"
        className="w-100"
      />
      <input
        onChange={(e) => setImageUrl(e.target.value)}
        value={imageUrl}
        type="text"
        className="w-100"
      />
      <div className="d-flex flex-row gap-2 p-2">
        <button className="btn btn-secondary flex-fill" type="submit">
          Add product
        </button>
        <button className="btn btn-secondary flex-fill" type="reset">
          Reset
        </button>
      </div>
    </form>
  );
};

export default ProductInput;
