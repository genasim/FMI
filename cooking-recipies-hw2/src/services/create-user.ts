import { User, UserDTO } from "../models/User";
import API, { Tables } from "../shared/api-client/ApiClient";

const createUser = async (entity: UserDTO) => {
  try {
    const user = await API.create<User>(Tables.USERS, entity);
    return user;
  } catch (error) {
    console.error(error);
  }
};

export default createUser;
