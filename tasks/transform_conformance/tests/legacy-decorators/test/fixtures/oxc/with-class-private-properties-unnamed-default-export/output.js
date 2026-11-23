let _default = { default: class {
  constructor() {
    babelHelpers.defineProperty(this, "prop", 0);
  }
  meth() {
    return this.prop;
  }
} }.default;
_default = babelHelpers.decorate([dec], _default);
export default _default;
