import { Identifiable } from "./Identifiable";
import { Role } from "./Role";
import { Sex } from "./Sex";
import { AccountStatus } from "./AccountStatus";

export interface UserDTO {
  name: string;
  username: string;
  password: string;
  sex: Sex;
  role: Role;
  imageUrl?: string;
  bio: string;
  status: AccountStatus;
  registerDate: Date;
  lastModDatetime: Date;
}

export interface User extends UserDTO, Identifiable {}
