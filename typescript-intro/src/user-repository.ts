import { Repository, RepositoryInMemory } from "./repository.js";
import { Optional } from "./types/shared-types.js";
import { IdType, User } from "./users.js";

// export type FindByString<V> = (str: string) => Optional<V>   both are
export interface FindByString<V> {
  (str: string): Optional<V>;
}

export interface UserRepository extends Repository<IdType, User> {
  findByEmail: FindByString<User>;
}

export class UserRepositoryInMemory
  extends RepositoryInMemory<IdType, User>
  implements UserRepository
{
  findByEmail(email: string): Optional<User> {
    return this.findAll().find((user) => user.email === email);
  }
}
