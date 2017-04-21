Require Export Basics.

Theorem plus_n_0 : forall n : nat, n = n + 0.
Proof.
  intros n. 
  induction n as [ | n' IHn'].
  - (*n = 0*) reflexivity.
  - (*n = S n'

    We need to show S n' = S n' + 0. By simplification the right
    hand side can be written S (n' + 0) and n' + 0 = n' by the
    induction hypothesis.

   *)
    simpl. rewrite <- IHn'. reflexivity. Qed.

Theorem minus_diag : forall n, minus n n = 0.
Proof.
  intros n.
  induction n as [ | n' IHn'].
  - (*n = 0*)
    simpl. reflexivity.
  - (*n = S n'*)
    simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem mult_0_r : forall n : nat, n * 0 = 0.
Proof.
  intros n.
  induction n as [ | n' IHn'].
  - (*n = 0*)
    simpl. reflexivity.
  - (*n = S n'

      We need to show that (S n') * 0 = 0.

      But, S n' * 0 = 0 + (n' * 0) = n' * 0 by simplification and n' *
      0 = 0 holds due to the induction hypothesis.

 *)
  simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_n_sm : forall n m : nat,
  S (n + m) = n + (S m).
Proof.
  intros n m.
  induction n as [ | n' IHn'].
  - (*n = 0*)
    simpl. reflexivity.

  - (*n = S n'
      S (n' + m) = n' + S m
      ----------------------
      S (S n' + m) = S n' + S m

     By simplification,

      S (S (n' + m)) = S (n' + S m).

      Then, by the induction hypothesis we can rewrite the right hand
      side of the above to get

      S (S (n' + m)) = S (S (n' + m))

      and the proof follows by reflexivity.
     *)
    simpl.
    rewrite <- IHn'.
    reflexivity.
Qed.

Theorem plus_comm : forall n m : nat,
  n + m = m + n.
Proof.
  intros n m.
  induction n as [ | n' IHn'].

  - (* n = 0

     By simplification, we get m = m + 0 and rewriting using
     [plus_n_0] we get m + 0 = m.
   *)
   simpl.
   rewrite <- plus_n_0.
   reflexivity.

   - (* n = S n'
        n' + m = m + n'
        ---------------
        S n' + m = m + S n'

        By simplification S n' + m = S (n' + m).
        By, induction hypothesis  S (n' + m) = S (m + n').
        Applying [plus_n_sm] we have
        S (m + n') = m + S n'.
      *)
      simpl.
      rewrite -> IHn'.
      rewrite <- plus_n_sm.
      reflexivity.
Qed.

Theorem plus_assoc : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p.
  induction n as [ | n' IHn' ].
  - 
   simpl.
   reflexivity.
  - 
    simpl.
    rewrite <- IHn'.
    reflexivity.
Qed.

Fixpoint double (n : nat) :=
 match n with
  | O => O
  | S n' => S (S (double n'))
end.

Lemma double_plus : forall n : nat, double n = n + n.
Proof.
  intros n.
  induction n as [ | n' IHn'].
  - 
    simpl.
    reflexivity.
  - 
    simpl.
    rewrite -> IHn'.
    rewrite <- plus_n_sm.
    reflexivity.
Qed.

Theorem evenb_S : forall n : nat,
  evenb (S n) = negb (evenb n).
Proof.
  intros n.
  induction n as [ | n' IHn'].
  -
    simpl.
    reflexivity.
  -
   rewrite -> IHn'.
   simpl.
   rewrite -> negb_involutive.
   reflexivity.
Qed.

(* Destruct
   - Proof proceeds by case on all of the possible syntatic forms of a variable.
   Induction
   - Proof proceeds by using the principle of induction over natural numbers.
*)

Theorem mult_0_plus' : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  assert (H : 0 + n = n). { reflexivity. }
  rewrite -> H.
  reflexivity. Qed.

Theorem plus_rearrange_firsttry : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  (*We just need to swap (n + m) for (m + n)... seems like plus_comm
  should do the trick!*)
  rewrite -> plus_comm.
  (*Doesn't work... Coq rewrote the wrong plus!*)
Abort.

Theorem plus_rearrange : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  assert (n + m = m + n) as H.
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.

Theorem plus_assoc' : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof. intros n m p. induction n as [ | n' IHn']. reflexivity.
       simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_assoc'' : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n as [ | n' IHn' ].
  - (*n = 0*)
    reflexivity.
  - (*n = S n'*)
    simpl. rewrite -> IHn'. reflexivity.
Qed.

(*Theorem : Addition is commutative.

  Proof. By induction on n.

  o First suppose n = 0. We must show

    0 + m = m + 0.

    By definition, 0 + m = m and m + 0 = m by an earlier proven result
    ([plus_n_0]). That is, 0 + m = m + 0 = m.
  
  o Next, suppose n = S n', where n' + m = m + n'.  We must show,
  
    S n' + m = m + S n'.

    By definition,

    S n' + m = S (n' + m) and by the induction hypothesis we have S
    (n' + m )= S (m + n').

    By an earlier result ([plus_n_sm]) S (m + n') = m + S n'. That is,
    S n' + m = m + S n' and the theorem is proved.
*)

(*Theorem : true = beq_nat n n for any n.

  Proof: By induction on n.

  o First suppose n = 0. We must show beq_nat 0 0 which holds by
    definition.

  o Now assume n = S n' and true = beq_nat n' n'. We must show, true
    beq_nat (S n') (S n').

    But, by definition, beq_nat (S n') (S n') holds if beq_nat n' n'
    which is assumed by the induction hypothisis.
*)

Theorem plus_swap : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  rewrite -> plus_comm.
  assert (n + p = p + n) as H. { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  rewrite <- plus_assoc.
  reflexivity.
Qed.


Theorem mult_n_0 : forall n : nat,
  n * 0 = 0.
Proof.
  intros n. induction n as [| n' IHn'].
  - (* n = 0 *) reflexivity.
  - (* n = S n'*)
    simpl.
    rewrite -> IHn'.
    reflexivity.
Qed.

Theorem mult_comm_lemma : forall n m : nat,
  m * S n = m + m * n.
Proof.
  intros n m.
  induction m as [ | m' IHm'].
  - (*m = 0*)
    simpl.
    reflexivity.
  - (*m = S m'*)
    simpl.
    rewrite -> plus_swap.
    rewrite -> IHm'.
    reflexivity.
Qed.

Theorem mult_comm : forall m n : nat,
   m * n = n * m.
Proof.
  intros m n. induction n as [| n' IHn' ].
  - (*n = 0*)
    simpl.
    rewrite -> mult_n_0.
    reflexivity.
  - (*n = S n'*)
    simpl.
    rewrite <- IHn'.
    rewrite -> mult_comm_lemma.
    reflexivity.
Qed.
