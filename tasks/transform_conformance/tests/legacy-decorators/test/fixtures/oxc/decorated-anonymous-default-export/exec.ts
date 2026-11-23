let decorated: any;
// The generated binding must not change the name, even when `_default` is taken.
const _default = "user";
function dec(target: any): any {
  decorated = target;
}

@dec
export default class {
  static value = 42;
  static #secret = 7;
  static self = this;
  static fromInitializer = this.#secret;
  static readSecret() {
    return this.#secret;
  }
  static {
    (this as any).fromBlock = this.value;
  }
  field = 1;
}

expect(decorated.name).toBe("default");
expect(_default).toBe("user");
expect(decorated.value).toBe(42);
expect(decorated.self).toBe(decorated);
expect(decorated.fromInitializer).toBe(7);
expect(decorated.readSecret()).toBe(7);
expect(decorated.fromBlock).toBe(42);
expect(new decorated().field).toBe(1);
