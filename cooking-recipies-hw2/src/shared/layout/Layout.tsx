import { FC, createContext, useEffect, useState } from "react";
import { Container } from "react-bootstrap";
import { Outlet } from "react-router-dom";
import Navbar from "./components/Navbar";

export const LogoutContext = createContext<{
  value: boolean;
  setLogout: (v: boolean) => void;
}>({ setLogout: () => false, value: false });

const Layout: FC = () => {
  const [showLogout, setShowLogout] = useState(true);
  // useEffect(() => {
    
  //   if (!sessionStorage.getItem("token") || !sessionStorage.getItem("user-name")) {
  //     sessionStorage.removeItem("token")
  //     sessionStorage.removeItem("user-name")
  //     setShowLogout(true)
  //   }
  // }, [])

  const handleTrigger = (value: boolean) => setShowLogout(value);

  return (
    <div className="d-flex flex-column min-vh-100">
      <LogoutContext.Provider value={{ value: showLogout, setLogout: handleTrigger }}>
        <Navbar />
        <Container className="flex-grow-1 p-4">
          <Outlet />
        </Container>
      </LogoutContext.Provider>
    </div>
  );
};

export default Layout;
