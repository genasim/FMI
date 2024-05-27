import { User } from "../models/User";
import API, { Tables } from "../shared/api-client/ApiClient";

const updateUser = async (user: User) => {
  try {
    const updated = await API.update<User>(Tables.USERS, user);
    return updated;
  } catch (error) {
    console.error(error);
  }
};

export default updateUser;
