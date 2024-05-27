import { LoaderFunction } from "react-router-dom";
import { User } from "../models/User";
import API, { Tables } from "../shared/api-client/ApiClient";

const fetchAllUsers: LoaderFunction = async () => {
  try {
    const users = await API.findAll<User>(Tables.USERS);
    users.map((user) => ({
      ...user,
      registerDate: user.registerDate as Date,
      lastModDatetime: user.lastModDatetime as Date,
    }));
    return users
  } catch (error) {
    console.error(error);
    return []
  }
};

export default fetchAllUsers;
