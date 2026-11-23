let decorated: any;
function dec(target: any, key: string, descriptor: PropertyDescriptor): any {
  decorated = target;
}

export default class {
  @dec static accessor value = 42;
  static #secret = 7;
  static self = this;
  // Reads the lowered accessor while the class is being evaluated.
  static fromInitializer = this.value;
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
expect(decorated.fromInitializer).toBe(42);
expect(decorated.readSecret()).toBe(7);
expect(decorated.fromBlock).toBe(42);
decorated.value = 43;
expect(decorated.value).toBe(43);
expect(new decorated().field).toBe(1);
