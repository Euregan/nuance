import { Elm } from "./Main.elm";

const node = document.getElementById("root");

if (node) {
  Elm.Main.init({
    node,
    flags: {
      size: [node.clientWidth, node.clientHeight],
    },
  });
}
