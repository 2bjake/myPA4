(*
class A {};
class Main {};
class IO {};
class SELF_TYPE {};
class B inherits SELF_TYPE {};
class B inherits Int {};
class B inherits Object {};
class C inherits D {};

class A1 inherits A4 {};
class A2 inherits A1 {};
class A3 inherits A2 {};
class A4 inherits A3 {};
class Object {};

class Main inherits IO {
    f(): Foo { 1 };
    main(): Int { 5 };
    val: Foo;
    self: Int;
};

class A {
    foot: Int;
    fun(): Int { 2 };
    merp(a: Int, b: Int): Int { 2 };
    chap(a: ASDF): Int { 2 };
};

class B inherits A {
    foot: Int;
    fun(): Bool { 2 };
    merp(a: Int, b: Bool): Int { 2 };
    erp(a: Int, a: Bool): Int { 2 };
};

class C {
    g(self: Int): Int { 2 };
    f(): SELF_TYPE { 5 };
    self(): SELF_TYPE { 4 };
};
*)

class Foo {};

class Bar inherits Foo {};

class Baz {};
(*
class Main {
    fo: Foo <- new Object;
    ff: Foo <- new Foo;
    fb: Foo <- new Bar;
    fz: Foo <- new Baz;
    iv: Bool <- isvoid 1 + ~2 < 6 * 12;
    jj: Foo <- new Jib;
    y: Bool <- false;
    i: Int <- ~true + 3;
    x: Foo <- new Foo;
    bar: Bool <- isvoid x;
    main(): Int { 5 };
};
*)

class Main {
    f: Int <- true;
    x: Int <- f;
    main(): Bool { true };
};