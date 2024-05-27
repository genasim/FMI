import { FC, useContext } from "react";
import { Navbar as BsNavbar, Container, Nav } from "react-bootstrap";
import { NavLink } from "react-router-dom";
import { LogoutContext } from "../Layout";

const Navbar: FC = () => {
  const { value: showLogout, setLogout } = useContext(LogoutContext);

  const handleLogout = () => {
    setLogout(false);
    sessionStorage.removeItem("token");
    sessionStorage.removeItem("user-name");
  };

  return (
    <BsNavbar expand="lg" className="bg-body-tertiary">
      <Container>
        <BsNavbar.Brand>Cooking Blog</BsNavbar.Brand>
        <div className="float-end">
          <BsNavbar.Toggle aria-controls="basic-navbar-nav" />
          <BsNavbar.Collapse id="basic-navbar-nav float-end">
            <Nav className="me-auto">
              <Nav.Link as="div">
                <NavLink className="text-decoration-none link-secondary" to="/">
                  Home
                </NavLink>
              </Nav.Link>
              <Nav.Link as="div">
                <NavLink
                  className="text-decoration-none link-secondary"
                  to="/users"
                >
                  Users
                </NavLink>
              </Nav.Link>
              <Nav.Link as="div">
                <NavLink
                  className="text-decoration-none link-secondary"
                  to="/recipes"
                >
                  Recipes
                </NavLink>
              </Nav.Link>
              {!showLogout && (
                <>
                  <Nav.Link as="div">
                    <NavLink
                      className="text-decoration-none link-secondary"
                      to="/login"
                    >
                      Login
                    </NavLink>
                  </Nav.Link>
                  <Nav.Link as="div">
                    <NavLink
                      className="text-decoration-none link-secondary"
                      to="/register"
                    >
                      Register
                    </NavLink>
                  </Nav.Link>
                </>
              )}
              {showLogout && (
                <Nav.Link as="div">
                  <NavLink
                    className="text-decoration-none link-secondary"
                    to="/"
                    onClick={handleLogout}
                  >
                    Logout
                  </NavLink>
                </Nav.Link>
              )}
            </Nav>
          </BsNavbar.Collapse>
        </div>
      </Container>
    </BsNavbar>
  );
};

export default Navbar;
