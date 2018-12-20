import ReactDOM from "react-dom";
import { makeMainRoutes } from "./routes";

const routes = makeMainRoutes();
ReactDOM.render(routes, document.getElementById("root"));
