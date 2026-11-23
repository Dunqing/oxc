let decorated: any;
function dec(target: any, key: string): any {
  decorated = target;
}

export default class {
  @dec static value = 42;
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
expect(decorated.value).toBe(42);
expect(decorated.self).toBe(decorated);
expect(decorated.fromInitializer).toBe(7);
expect(decorated.readSecret()).toBe(7);
expect(decorated.fromBlock).toBe(42);
expect(new decorated().field).toBe(1);
