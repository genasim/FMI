import { LoaderFunction } from "react-router-dom";
import { User } from "../models/User";
import API, { Tables } from "../shared/api-client/ApiClient";

const fetchUserId: LoaderFunction = async ({ params }) => {
  const { userId } = params;
  try {
    const user = await API.findById<User>(Tables.USERS, userId ?? "");
    return user;
  } catch (error) {
    console.error(error);
  }
};

export default fetchUserId;
