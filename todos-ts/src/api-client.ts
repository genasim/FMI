import { IdType, Identifiable } from "./shared-types.js";

interface EntityContructor<V> {
  new (...args: any): V;
  className: string;
}

export class ApiClient {
  constructor(private baseUrl: string) {}

  async findAll<V extends Identifiable<IdType>>(
    ctor: EntityContructor<V>
  ): Promise<V[]> {
    return await this.fetchData(`${this.baseUrl}/${ctor.className.toLowerCase()}s`);
  }
  // async findById(id: K) {
  //     const found = this.entities.get(id);
  //     if (found) {
  //         return found;
  //     }
  //     throw new Error(`Entity with ID='${id}' not found.`);
  // }

  // async create(entity: Omit<V, 'id'>) {
  //     const result = entity as V;
  //     result.id = this.idGen.getNextId();
  //     this.entities.set(result.id, result);
  //     return result;
  // }
  // async update(entity: V) {
  //     await this.findById(entity.id);
  //     this.entities.set(entity.id, entity);
  //     return entity;
  // }
  // async deleteById(id: K) {
  //     const exisitng = await this.findById(id);
  //     this.entities.delete(id);
  //     return exisitng;
  // }
  // get size() {
  //     return Promise.resolve(this.entities.size);
  // }

  private async fetchData<D>(uri: string, options?: RequestInit): Promise<D> {
    const res = await fetch(uri, options);
    if (res.status >= 400)
        throw new Error(await res.text())
    return res.json();
  }
}
