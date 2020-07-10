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
*)

class Main inherits IO {
    f(): Foo { 1 };
    main(): Int { 5 };
};
