import { UserRepository, UserRepositoryInMemory } from "./user-repository.js";
import { Role, User, UserDto } from "./users.js";
import { IdGenerator } from "./types/id-generator.js"


const users: UserCreateDTO[] = [
    new UserDto('John', 'Doe', 'john@gmail.com', 'john123', { country: 'BG', city: 'Sofia', address: 'J. Bouchoier, 12' },
        [Role.Reader, Role.Author, Role.Admin]),
    new UserDto('Jane', 'Doe', 'jane@gmail.com', 'jane123', { country: 'GB', }, [Role.Reader, Role.Author]),
    new UserDto('Ivan', 'Petrov', 'ivan@gmail.com', 'ivan123'),
    new UserDto('Kain', 'Petrov', 'kalin@abv.bg', 'kalin1234')
];

type UserCreateDTO = Omit<User, "id">

class IdGeneratorNumber implements IdGenerator<number> {
    private id: number = 0
    getNextId(): number {
        return ++this.id
    }
}

const userRepo: UserRepository = new UserRepositoryInMemory(new IdGeneratorNumber())
users.forEach(user => userRepo.create(user))

const contentElem = document.getElementById('content');
const usersItemsStr = userRepo.findAll().map(user => `<li>${user.salutation}</li>`).join('');
if (contentElem) {
    contentElem.innerHTML = `<ul>${usersItemsStr}</ul>`;
}