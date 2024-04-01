import { IdGenerator } from "./types/id-generator.js";
import { Identifiable, Optional } from "./types/shared-types.js";

export interface Repository<K, V extends Identifiable<K>> {
  findAll(): V[];
  findById(id: K): Optional<V>;
  create(entity: Omit<V, "id">): V;
  update(entity: V): Optional<V>;
  deleteById(id: K): Optional<V>;
  readonly size: number;
}

export class RepositoryInMemory<K, V extends Identifiable<K>>
  implements Repository<K, V>
{
  private entities = new Map<K, V>();

  constructor(private idGen: IdGenerator<K>) {}

  findAll(): V[] {
    return Array.from(this.entities.values());
  }

  findById(id: K): Optional<V> {
    return this.entities.get(id);
  }

  create(entity: Omit<V, "id">): V {
    const result = entity as V;
    result.id = this.idGen.getNextId();
    this.entities.set(result.id, result);

    return result;
  }

  update(entity: V): Optional<V> {
    const existing = this.findById(entity.id);
    if (existing) {
      this.entities.set(entity.id, entity);
      return entity;
    }
    return undefined;
  }

  deleteById(id: K): Optional<V> {
    const existing = this.findById(id);
    if (existing) {
      this.entities.delete(id);
      return existing;
    }
    return undefined;
  }

  get size(): number {
    return this.entities.size;
  }
}
