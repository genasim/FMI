// Simulate enums in JS
const ADMIN = 0
const AUTHOR = 1
const READER = 2

const Role = ['ADMIN', 'AUTHOR', 'READER']



class Person {
    static nexId = 0
    id = ++Person.nexId

    constructor(name, surname, address) {
        this.name = name
        this.surname = surname
        this.address = address
    }

    toString() {
        return `${this.id}: ${this.name} ${this.surname}, ${this.address}`
    }

    foo = () => {
        console.log(this.foo);
    }
}

class User extends Person {
    constructor(name, surname, address, username, password, role) {
        super(name, surname, address)
        this.username = username
        this.password = password
        this.role = role
    }

    toString() {
        return `${super.toString()}, ${this.username} ${this.password} -> ${Role[this.role]}`
    }
}

const users = [
    new User("Test", "Testov", "ul. 1", "testing", "1234", READER),
    new User("Testka", "Testova", "ul. 2", "testing", "1234", ADMIN),
    new Person("Bat", "Miro", "ul. 2"),
]

for (user of users) {
    console.log(user.toString());
}

users[0].foo()