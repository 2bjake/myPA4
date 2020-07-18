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
(*
class O {};
class A inherits O {};
class B inherits A {};
class C inherits B {};
class AA inherits A {};
class AB inherits AA {};

class Main {
    f: Int <- true;
    x: Int <- if true then new C else new Object fi;
    main(): Bool { true };
    b: Int <- {
        true;
        false = 1;
        2 + 3;
        x <- 67;
        "hi";
    };
    c: Int <- let x: Int <- x in x;
    d: Int <- x.doThing(1, 2, 3);
    e: IO <- new IO;
    g: Object <- e.out_int(5);
};
*)

class A {};
class B inherits A {};
class C {
    func2(): SELF_TYPE { new SELF_TYPE };
};

class C1 inherits C {
    func(): SELF_TYPE { new C1 };
};

class Main {
    func(a: Main): SELF_TYPE { true };
    var: SELF_TYPE <- func(new SELF_TYPE);
    v: C1 <- new C1@C1.func();
    v2: C1 <- new C1@C.func2();
    main(): Bool { true };
};