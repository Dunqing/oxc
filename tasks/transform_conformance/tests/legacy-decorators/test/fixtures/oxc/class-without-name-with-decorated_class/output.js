import { dec } from "dec";
let _default = { default: class {
  foo = 0;
} }.default;
babelHelpers.decorate([dec], _default.prototype, "foo", void 0);
_default = babelHelpers.decorate([dec], _default);
export default _default;
