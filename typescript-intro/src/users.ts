type IdType = number;
type CoutnryType = "BG" | "US" | "GR" | "EN" | "DE";

export interface Person {
  id: IdType;
  firstName: string;
  lastName: string;
  email: string;
  contact?: Contact;
}

export interface Contact {
  country: CoutnryType;
  city?: string;
  address?: string;
  phone?: string;
}

export enum Role {
  Author = 0,
  Reader,
}

export interface User extends Person {
  password: string;
  roles: Role[]; // same as Array<Role>
}

export class UserBase implements User {
  constructor(
    public password: string,
    public roles: Role[],
    public id: number,
    public firstName: string,
    public lastName: string,
    public email: string,
    public contact?: Contact | undefined
  ) {}
}
