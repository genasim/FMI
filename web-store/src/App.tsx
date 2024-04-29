import "bootstrap/dist/css/bootstrap.min.css";
import { useState } from "react";
import "./App.css";
import ProductFilter from "./components/ProductFilter";
import ProductInput from "./components/ProductInput";
import ProductList from "./components/ProductList";
import useAsyncEffect from "./hooks/useAsyncEffect";
import { Product, ProductCategory, ProductDTO, ProductOrder } from "./models/product";
import API from "./services/api-client";
import { Optional } from "./shared/shared-types";

function App() {
  const [products, setProducts] = useState<Product[]>([]);
  const [filterCategory, setFilterCategory] = useState<ProductCategory>();
  const [order, setOrder] = useState<ProductOrder>();
  const [error, setError] = useState<Error>();

  useAsyncEffect(async () => {
    const data = await API.findAll(Product);
    setProducts(data);
  }, []);

  const handleProductCreate = async (data: ProductDTO) => {
    try {
      const product = await API.create(Product, data);
      setProducts([...products, product]);
    } catch (error) {
      setError(error as Error);
    }
  };

  const handleProductDelete = async (data: Product) => {
    try {
      const deleted = await API.deleteById(Product, data.id);
      setProducts(products.filter((prod) => prod.id !== deleted.id));
    } catch (error) {
      setError(error as Error);
    }
  };

  const handleOnError = (error: Error) => {
    setError(error);
  };

  const handleFilterChange = (filter: Optional<ProductCategory>) => {
    setFilterCategory(filter);
  };

  const handleOrderChange = (order: ProductOrder) => {
    setOrder(order)
  }

  const handleProductEdit = async (data: Product) => {
    try {
      const updated = await API.update(Product, data);
      setProducts((products) =>
        products.map((product) =>
          product.id === updated.id ? updated : product
        )
      );
    } catch (error) {
      setError(error as Error);
    }
  };

  return (
    <section>
      <h2 className="text-primary text-center">
        Web store management platform demo
      </h2>
      <hr />
      <div className="container my-5">
        <ProductInput onCreate={handleProductCreate} onError={handleOnError} />
        {error && <h2 className="my-2">{error.message}</h2>}
        <ProductFilter order={order} onChangeOrder={handleOrderChange} category={undefined} onChangeCategory={handleFilterChange} />
        <ProductList
          products={products}
          order={order}
          filter={filterCategory}
          onDelete={handleProductDelete}
          onEdit={handleProductEdit}
        />
      </div>
    </section>
  );
}

export default App;
