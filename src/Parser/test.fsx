let str = """module pidtt.exchange (A: Type) where {
    open import primdtt.foundation public;
    module _
        (A: Type) (B: el A → Type)
        where {            
            Π : ⇒ Type;
            abs : (b : (x : el A) → el (B x)) ⇒ el (Π);
            app : (p : el (Π A B)) (a : el A) ⇒ el (B a);
            (b : (x : el A) → el (B x)) (a : el A) ⇒ app(abs(b), a) = b a
        }
    }"""

let str2 = """module pidtt.foundation where{
  open import primdtt.foundation public;
  module _
    (A : Type) (B : el A → Type)
    where{

    Π : ⇒ Type;
    abs : (b : (x : el A) → el (B x)) ⇒ el (Π);
    app : (p : el (Π A B)) (a : el A) ⇒ el (B a);
    (b : (x : el A) → el (B x)) (a : el A) ⇒ app(abs(b), a) = b a;
    (p : el Π) ⇒ abs(λ (a : el A) → app(p, a)) = p
}}"""

let prmdtt_foundation =
    """module primdtt.foundation where {
        Type : ⇒ □;
        el : (A : Type) ⇒ *
    }
"""

let idtype = """module primdtt.foundation where {
        Type : ⇒ □;
        el : (A : Type) ⇒ ∗;
        Id : (A : Type) (a : el(A)) (b : el(A)) ⇒ Type;
        refl : (A : Type) (a : el(A)) ⇒ el(Id(A, a, a));
        indId : (A : Type) (a : el(A)) (b : el(A)) (p : el(Id(A, a, b))) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a))))⇒ el(C(b, p));
        _eq: (A : Type) (a : el(A)) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a)))) ⇒ indId(A, a, a, refl(A, a), C, c) = c
    }
"""

let pitype = """module primdtt.foundation where {
        Type : ⇒ □;
        el : (A : Type) ⇒ ∗;
        Id : (A : Type) (a : el(A)) (b : el(A)) ⇒ Type;
        refl : (A : Type) (a : el(A)) ⇒ el(Id(A, a, a));
        indId : (A : Type) (a : el(A)) (b : el(A)) (p : el(Id(A, a, b))) (C : (x : el(A), y : el(Id(A, a, x))) → Type) (c : el(C(a,refl(A, a)))))⇒ el(C(b, p));
        _eq: (A : Type) (a : el(A)) (C : (x : el(A)) (y : el(Id(A, a, x))) → Type, c : el(C(a,refl(A, a)))) ⇒ indId(A, a, a,refl(A, a), C, c) = c
    }"""