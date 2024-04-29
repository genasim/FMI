import { FC, FormEvent, useCallback, useState } from "react";
import { Button, Modal } from "react-bootstrap";
import { Product, ProductCategory } from "../models/product";

interface EditProductProps {
  product: Product;
  onUpdate: (product: Product) => void;
}

const EditProduct: FC<EditProductProps> = ({ product, onUpdate }) => {
  const [show, setShow] = useState(false);

  const handleClose = () => setShow(false);
  const handleShow = () => setShow(true);

  const [title, setTitle] = useState<string>(product.name);
  const [details, setDetails] = useState<string>(product.details);
  const [category, setCategory] = useState<ProductCategory>(product.category);
  const [price, setPrice] = useState<number>(product.price);

  const handleSubmit = useCallback(
    (event: FormEvent) => {
      event.preventDefault();

      const updatedProduct: Product = {
        id: product.id,
        name: title,
        details,
        price,
        category,
        imageUrl: product.imageUrl,
      };

      onUpdate(updatedProduct);
      handleClose()
    },
    [product.id, product.imageUrl, title, details, price, category, onUpdate]
  );

  return (
    <>
      <Button variant="success" onClick={handleShow}>
        Edit
      </Button>

      <Modal show={show} onHide={handleClose}>
        <form onSubmit={handleSubmit} className="my-5">
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
            <select value={category} onChange={(e) => setCategory(parseInt(e.target.value))}>
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
          <div className="d-flex flex-row gap-2 p-2">
            <button className="btn btn-secondary flex-fill" type="submit">
              Edit product
            </button>
          </div>
        </form>
      </Modal>
    </>
  );
};

export default EditProduct;
