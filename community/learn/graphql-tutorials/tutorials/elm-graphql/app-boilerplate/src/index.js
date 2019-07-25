import './main.css';
import { Elm } from './Main.elm';

document.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    node: document.getElementById("root")
  });
})

