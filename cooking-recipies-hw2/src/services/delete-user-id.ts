import { IdType } from "../models/Identifiable";
import { User } from "../models/User";
import API, { Tables } from "../shared/api-client/ApiClient";

const deleteUserId = async (id: IdType) => {
  try {
    const deleted = await API.deleteById<User>(Tables.USERS, id);
    return deleted;
  } catch (error) {
    console.error(error);
  }
};

export default deleteUserId;
