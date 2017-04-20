Inductive day : Type :=
  | monday : day
  | tuesday : day
  | wednesday : day
  | thursday : day
  | friday : day
  | saturday : day
  | sunday : day.

Definition next_weekday (d : day) : day :=
  match d with
  | monday => tuesday
  | tuesday => wednesday
  | wednesday => thursday
  | thursday => friday
  | friday => monday
  | saturday => monday
  | sunday => monday
  end.

Compute (next_weekday friday).
Compute (next_weekday (next_weekday saturday)).

Example test_next_weekday : 
  (next_weekday (next_weekday saturday)) = tuesday.

Proof. simpl. reflexivity. Qed.

Inductive bool : Type :=
  | true : bool
  | false : bool.

Definition negb (b : bool) : bool :=
  match b with
    | true => false
    | false => true
  end.

Definition andb (a : bool) (b : bool) : bool :=
  match a with
    | true => b
    | false => false
  end.

Definition orb (a : bool) (b : bool) : bool :=
  match a with
    | true => true
    | false => b
  end.

Example test_orb1 : (orb true false) = true.
Proof. simpl. reflexivity. Qed.
Example test_orb2 : (orb false false) = false.
Proof. simpl. reflexivity. Qed.
Example test_orb3 : (orb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_orb4 : (orb true true) = true.
Proof. simpl. reflexivity. Qed.

Infix "&&" := andb.
Infix "||" := orb.

Example test_orb5 : false || false || true = true.
Proof. simpl. reflexivity. Qed.

(*[nandb b1 b2] should return [true] if either of both it's input are
[false]*)
Definition nandb (b1:bool) (b2:bool) : bool :=
  match (b1, b2) with
    | (true, true) => false
    | (false, false) => true
    | (true, false) => true
    | (false, true) => true
  end.


Example test_nandb1: (nandb true false) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb2: (nandb false false) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb3: (nandb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb4: (nandb true true) = false.
Proof. simpl. reflexivity. Qed.

Definition andb3 (b1 : bool) (b2 : bool) (b3 : bool) : bool :=
  match (b1, b2, b3) with
    | (true, true, true) => true
    | _ => false
  end.

Example test_andb31: (andb3 true true true) = true.
Proof. simpl. reflexivity. Qed.

Example test_andb32: (andb3 false true true) = false.
Proof. simpl. reflexivity. Qed.

Example test_andb33: (andb3 true false true) = false.
Proof. simpl. reflexivity. Qed.

Example test_andb34: (andb3 true true false) = false.
Proof. simpl. reflexivity. Qed.

Check true.
Check (negb true).
Check (negb).

Module NatPlayground.

  Inductive nat : Type : Type :=
  | O : nat
  | S : nat -> nat.

  Definition pred (n : nat) : nat :=
  match n with
  | O => O
  | S n' => n'
  end.

End NatPlayground.

Definition minustwo (n : nat) : nat :=
  match n with
  | O => O
  | S O => O
  | S (S n') => n'
end.

Check (S (S (S (S 0)))).
Compute (minustwo 4).

Check S.
Check pred.
Check minustwo.

Fixpoint evenb (n : nat) : bool :=
  match n with
  | O => true
  | S O => false
  | S (S n') => evenb n'
  end.

Definition oddb (n : nat) : bool := negb (evenb n).

Example test_oddb1 : oddb 1 = true.
Proof. simpl. reflexivity. Qed.
Example test_oddb2 : oddb 4 = false.
Proof. simpl. reflexivity. Qed.

Module NatPlayground2.

Fixpoint plus (n : nat) (m : nat) : nat :=
  match n with
    | O => m
    | S n' => S (plus n' m)
  end.

Fixpoint mult (n m : nat) : nat :=
  match n with
  | O => O
  | S n' => plus m (mult n' m)
  end.

Example test_mult1 : (mult 3 3) = 9.
Proof. simpl. reflexivity. Qed.

Fixpoint minus (n m : nat) : nat :=
  match n, m with
    | O, _ => O
    | S _, O => n
    | S n', S m' => minus n' m'
   end.

Fixpoint exp (a n : nat) : nat :=
  match n with
    | O => S O
    | S n' => mult a (exp a n')
end.

End NatPlayground2.

Fixpoint factorial (n : nat) : nat :=
  match n with
  | O => S O
  | S n' => mult n (factorial n')
  end.

Example test_factorial1 : (factorial 3) = 6.
Proof. simpl. reflexivity. Qed.
Example test_factorial2 : (factorial 5) = (mult 10 12).
Proof. simpl. reflexivity. Qed.

Notation "x + y" := (plus x y)
                      (at level 50, left associativity) 
                      : nat_scope.

Notation "x - y" := (minus x y)
                      (at level 50, left associativity) 
                      : nat_scope.

Notation "x * y" := (mult x y)
                      (at level 40, left associativity) 
                      : nat_scope.

Fixpoint beq_nat (n m : nat) : bool :=
  match n with
    | O => match m with
           | O => true
           | _ => false
           end
    | S n' => match m with
              | S m' => beq_nat n' m'
              | _ => false
              end
  end.

Fixpoint leb (n m : nat) : bool :=
  match n with
    | O => true
    | S n' => 
      match m with
        | O => false
        | S m' => leb n' m'
      end
  end.

Example test_leb1 : (leb 2 2) = true.
Proof. simpl. reflexivity. Qed.
Example test_leb2 : (leb 2 4) =  true.
Proof. simpl. reflexivity. Qed.
Example test_leb3 : (leb 4 2) = false.
Proof. simpl. reflexivity. Qed.

Definition blt_nat (n m : nat) : bool :=
  andb (leb n m) (negb (beq_nat n m)).

Example test_blt_nat1 : (blt_nat 2 2) = false.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat2 : (blt_nat 2 4) = true.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat3 : (blt_nat 4 2) = false.
Proof. simpl. reflexivity. Qed.

Theorem plus_O_n : forall n : nat, 0 + n = n.
Proof.
  intros n. simpl. reflexivity. Qed.

Theorem plus_O_n' : forall n : nat, 0 + n = n.
Proof.
  intros n. reflexivity. Qed.

Theorem plus_1_l : forall n : nat, 1 + n = S n.
Proof.
  intros n. simpl. reflexivity. Qed.

Theorem mult_0_1 : forall n : nat, 0 * n = 0.
Proof.
  intros n. simpl. reflexivity. Qed.

Theorem plus_n_O : forall n, n = n + 0.
Proof.
  intros n. simpl. Abort.

Theorem plus_id_example : forall n m : nat,
  n = m -> n + n = m + m.

Proof.
  (*Move both quantifiers into the context*)
  intros n m.
  (*Move the hypothesis into the context*)
  intros H.
  (*Rewrite the goal using the hypothesis*)
  rewrite <- H.
  reflexivity. Qed.

Theorem plus_id_exercise : forall n m o : nat,
  n = m -> m = o -> n + m = m + o.

Proof.
  intros n m o.
  intros .
  rewrite -> H.
  rewrite <- H0.
  reflexivity.
  Qed.

Theorem mult_0_plus : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  rewrite -> plus_O_n.
  reflexivity.
  Qed.

Theorem mult_S_1 : forall n m : nat,
  m = S n -> m * (1 + n) = m * m.
Proof.
  intros n m.
  intros H.
  rewrite -> plus_1_l.
  rewrite -> H.
  reflexivity.
  Qed.

Theorem plus_1_neq_O_firsttry : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n. destruct n as [ | n'].
  - simpl. reflexivity.
  - simpl. reflexivity. Qed.

Theorem negb_involutive : forall b : bool,
  negb (negb b) = b.
Proof.
  intros b. destruct b as [ | ].
  - reflexivity.
  - reflexivity. Qed.

Theorem andb_commutative : forall b c, 
  andb b c = andb c b.
Proof.
  intros b c. destruct b.
  - destruct c.
    + reflexivity.
    + reflexivity.
  - destruct c.
    + reflexivity.
    + reflexivity.
Qed.

Theorem andb_commutative' : forall b c, 
  andb b c = andb c b.
Proof.
  intros b c. destruct b.
  { destruct c.
    { reflexivity. }
    { reflexivity. } }
  { destruct c.
    { reflexivity. }
    { reflexivity. } }
Qed.

Theorem andb_exchange :
  forall b c d, andb (andb b c) d = andb (andb b d) c.
Proof.
  intros b c d. destruct b.
  - destruct c.
  {  destruct d.
     - reflexivity.
     - reflexivity. }
  {  destruct d.
     - reflexivity.
     - reflexivity. }
  - destruct c.
  {  destruct d.
     - reflexivity.
     - reflexivity. }
  {  destruct d.
     - reflexivity.
     - reflexivity. }
  Qed.

Theorem plus_1_neq_0' : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros [ | n].
  - reflexivity.
  - reflexivity.
Qed.

Theorem andb_commutative'' :
  forall b c, andb b c = andb c b.
Proof.
  intros [] [].
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

Lemma orb_on_same : forall b, orb b b = b.
Proof.
  intros b.
  destruct b.
  - reflexivity.
  - reflexivity.
Qed.

Lemma orb_always_true :
  forall b,
    orb true b = true.
Proof. reflexivity. Qed.

Lemma andb_always_false : forall b, andb false b = false.
Proof.
  reflexivity.
Qed.

Theorem andb_eq_orb2' :
  forall b c, andb b c = orb b c -> b = c.
Proof.
Abort.  

Theorem andb_true_elim1 : forall b c :bool,
  andb b c = true -> c = true.
Proof.
  intros .
  destruct b.
  {(*b = true*)
    rewrite <- H.
    simpl.
    reflexivity.
  }
  {(*b = false*)
    destruct c.
    {(*c = true*)
      rewrite <- H.
      simpl.
      reflexivity.
    }
    {
      rewrite <- H.
      simpl.
      reflexivity.
    }
  }
Qed.

Theorem zero_nbegq_plus_1 : forall n : nat,
  beq_nat 0 (n + 1) = false.
Proof.
  intros n.
  destruct n as [ | n'].
  - reflexivity.
  - reflexivity.
Qed.

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

Theorem mult_0_plus' : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  assert (H : 0 + n = n). { reflexivity. }
  rewrite -> H.
  reflexivity. Qed.