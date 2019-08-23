const getHeaders = () => {
  const token = localStorage.getItem("auth0:id_token");
  const headers = {
    authorization: token ? `Bearer ${token}` : ""
  };
  return headers;
};

export { getHeaders };
