- Two styles of unification (`mgu` and `match`)
- There only exists trivial type equality (E.g., tycon matches tycon iff
  their ctors and args match).
- Two dicts for storing information during type inference:
  * `Gamma`: `Map String Scheme`, maps variables to their current scheme, E.g.:
      + `(+) :: Int -> Int -> Int`
      + `id :: Forall a. a -> a`
      + `head :: Forall a. [a] -> a`
  * `Subst`: `Map TyVar Type`, maintains the current substitution during
    the unification process.

- How to do type infererence
  * For literal, we just assign their types to the result
  * For variable, we lookup the type scheme in the Gamma and instanitiate
    the scheme.
  * For application `f a`, we make a fresh tyvar `t` and unify
    `typeOf(a) -> t` with `typeOf(f)`.
  * For non-recursive let, we first type inference each binder. Then,
    we substitute generic tyvars in the binders to make the binding
    polymorphic. Finally, we update the Gamma by those new bindings
    and type inference the body of let.
  * For recursive let, still not done yet...

