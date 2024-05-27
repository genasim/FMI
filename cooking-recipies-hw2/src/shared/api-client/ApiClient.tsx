import { IdType, Identifiable } from "../../models/Identifiable";

export enum Tables {
  USERS = "users",
  RECIPES = "recipes",
}

export class ApiClient {
  constructor(private baseUrl: string) {}

  findAll<V extends Identifiable>(table: string): Promise<Array<V>> {
    return this.fetchData(`${this.baseUrl}/${table}`);
  }

  findById<V extends Identifiable>(table: string, id: IdType): Promise<V> {
    return this.fetchData(`${this.baseUrl}/${table.toLocaleLowerCase()}/${id}`);
  }

  create<V extends Identifiable>(
    table: string,
    entity: Omit<V, "id">
  ): Promise<V> {
    return this.fetchData(`${this.baseUrl}/${table.toLocaleLowerCase()}`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(entity),
    });
  }

  update<V extends Identifiable>(table: string, entity: V): Promise<V> {
    return this.fetchData(
      `${this.baseUrl}/${table.toLocaleLowerCase()}/${entity.id}`,
      {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(entity),
      }
    );
  }

  deleteById<V extends Identifiable>(table: string, id: IdType): Promise<V> {
    return this.fetchData(
      `${this.baseUrl}/${table.toLocaleLowerCase()}/${id}`,
      {
        method: "DELETE",
      }
    );
  }

  private async fetchData<D>(uri: string, options?: RequestInit): Promise<D> {
    const resp = await fetch(uri, options);
    if (resp.status >= 400) {
      throw new Error(await resp.text());
    }
    return resp.json();
  }
}

const API = new ApiClient("http://localhost:3001");

export default API;
