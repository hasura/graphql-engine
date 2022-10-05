import AddUser from "./components/AddUser";
import EditUser from "./components/EditUser";
import UsersList from "./components/UsersList";
const router = [
     {
          path: "/",
          component: UsersList,
          name: "UsersList",
          default: true
     },
     {
          path: "/add",
          component: AddUser,
          name: "AddUser",
          default: true
     },
     {
          path: "/edit",
          component: EditUser,
          name: "EditUser",
          default: true
     },
];

export default router;