Require Export Induction.
Module NatList.

Inductive natprod : Type :=
  | pair : nat -> nat -> natprod.

Check (pair 3 5).

Definition fst (p : natprod) : nat :=
  match p with
    | pair x y => x
  end.

Definition snd (p : natprod) : nat :=
  match p with
    | pair x y => y
  end.

Notation "( x , y )" := (pair x y).

Compute (fst (3, 5)).

Definition fst' (p : natprod) : nat :=
  match p with
    | (x, y) => x
  end.

Definition snd' (p : natprod) : nat :=
  match p with
    | (x, y) => y
  end.

Definition swap_pair (p : natprod) : natprod :=
  match p with
    | (x,y) => (y,x)
  end.

Theorem surjective_pairing' : forall (n m : nat),
  (n, m) = (fst (n, m), snd (n, m)).
Proof.
  simpl.
  reflexivity.
Qed.

Theorem surjective_pairing_stuck : forall p : natprod,
    p = (fst p, snd p).
Proof.
  simpl.
Abort.

Theorem surjective_pairing : forall p : natprod,
  p = (fst p, snd p).
Proof.
  intros p.
  destruct p as [n m].
  simpl.
  reflexivity.
Qed.

Theorem snd_fst_is_swap : forall p : natprod,
  (snd p, fst p) = swap_pair p.
Proof.
  intros p.
  destruct p as [n m].
  simpl.
  reflexivity.
Qed.

Theorem fst_swap_is_snd : forall p : natprod,
  fst (swap_pair p) = snd p.
Proof.
  intros p.
  destruct p as [n m].
  simpl.
  reflexivity.
Qed.

Inductive natlist : Type :=
  | nil : natlist
  | cons : nat -> natlist -> natlist.

Definition mylist := cons 1 (cons 2 (cons 3 nil)).

Notation "x :: l" := (cons x l)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Fixpoint repeat (n count : nat) : natlist :=
  match count with
    | O => nil
    | S count' => n :: (repeat n count')
  end.

Fixpoint length (l : natlist) : nat :=
  match l with
    | nil => O
    | h :: t => S (length t)
  end.

Fixpoint app (l : natlist) (m : natlist) : natlist :=
  match l with
    | nil => m
    | h :: t => h :: app t m
end.

Notation "x ++ y" := (app x y)
                     (right associativity, at level 60).

Example test_app1 : [1; 2; 3] ++ [4; 5] = [1; 2; 3; 4; 5].
Proof. reflexivity. Qed.
Example test_app2 : nil ++ [4; 5] = [4; 5].
Proof. reflexivity. Qed.

Example test_app3 : [1; 2; 3] ++ nil = [1; 2; 3].
Proof. reflexivity. Qed.

Definition hd (default : nat) (l : natlist) : nat :=
  match l with
    | nil => default
    | h :: t => h
end.

Definition tl (l : natlist) : natlist :=
  match l with
    | nil => nil
    | h :: t => t
  end.

Example test_hd1 : hd 0 [1; 2; 3] = 1.
Proof. reflexivity. Qed.
Example test_hd2 : hd 0 [] = 0.
Proof. reflexivity. Qed.
Example test_tl : tl [1; 2; 3] = [2; 3].
Proof. reflexivity. Qed.

Fixpoint nonzeros (l : natlist) : natlist :=
  match l with
    | nil => nil
    | 0 :: tl => nonzeros tl
    | h :: tl => h :: (nonzeros tl)
  end.

Example test_nonzeros:
  nonzeros [0; 1; 0; 2; 3; 0; 0] = [1; 2; 3].
Proof. reflexivity. Qed.

Fixpoint oddmembers (l : natlist) : natlist :=
  match l with
    | nil => nil
    | h :: t =>
      match evenb h with
        | true => oddmembers t
        | false => h :: (oddmembers t)
      end
  end.

Example test_oddmembers:
  oddmembers [0; 1; 0; 2; 3; 0; 0] = [1; 3].
Proof. reflexivity. Qed.

Definition countoddmembers (l : natlist) : nat := length (oddmembers l).

Example test_countoddmembers1:
  countoddmembers [1; 0; 3; 1; 4; 5] = 4.
Proof. reflexivity. Qed.

Example test_countoddmembers2:
  countoddmembers [0; 2; 4] = 0.
Proof. reflexivity. Qed.

Example test_countoddmemebers3:
  countoddmembers nil = 0.
Proof. reflexivity. Qed.

Fixpoint alternate (l1 l2 : natlist) : natlist :=
  match l1 with
    | nil => l2
    | h :: tl =>
      match l2 with
        | nil => h :: tl
        | h' :: tl' => h :: h' :: (alternate tl tl')
      end
  end.

Example test_alternate1:
  alternate [1; 2; 3] [4; 5; 6] = [1; 4; 2; 5; 3; 6].
Proof. reflexivity. Qed.

Example test_alternate2:
  alternate [1] [4; 5; 6] = [1; 4; 5; 6].
Proof. reflexivity. Qed.

Example test_alternate3:
  alternate [1; 2; 3] [4] = [1; 4; 2; 3].
Proof. reflexivity. Qed.

Example test_alternate4:
  alternate [] [20; 30] = [20; 30].
Proof. reflexivity. Qed.

Definition bag := natlist.

Fixpoint count (v : nat) (s : bag) : nat :=
  match s with
    | nil => 0
    | h :: t =>
      match beq_nat v h with
        | true => 1 + (count v t)
        | false => (count v t)
      end
  end.

Example test_count1 : count 1 [1; 2; 3; 1; 4; 1] = 3.
Proof. reflexivity. Qed.

Example test_count2 : count 6 [1; 2; 3; 1; 4; 1] = 0.
Proof. reflexivity. Qed.

Definition sum : bag -> bag -> bag := app.

Example test_sum1 : count 1 (sum [1; 2; 3] [1; 4; 1]) = 3.
Proof. reflexivity. Qed.

Definition add (v : nat) (s : bag) : bag := v :: s.

Example test_add1 : count 1 (add 1 [1; 4; 1]) = 3.
Proof. reflexivity. Qed.
Example test_add2 : count 5 (add 1 [1; 4; 1]) = 0.
Proof. reflexivity. Qed.

Definition member (v : nat) (s : bag) : bool :=
  negb (beq_nat (count v s) 0).

Example test_member1 : member 1 [1; 4; 1] = true.
Proof. reflexivity. Qed.
Example test_member2 : member 2 [1; 4; 1] = false.
Proof. reflexivity. Qed.

Fixpoint remove_one (v : nat) (s : bag) : bag :=
  match s with
    | nil => nil
    | h :: t =>
      match beq_nat v h with
        | true => t
        | false => h :: remove_one v t
      end
  end.

Example test_removeone1:
  count 5 (remove_one 5 [2; 1; 5; 4; 1]) = 0.
Proof. reflexivity. Qed.

Example test_remove_one2:
  count 5 (remove_one 5 [2; 1; 4; 1]) = 0.
Proof. reflexivity. Qed.

Example test_remove_one3:
  count 4 (remove_one 5 [2; 1; 4; 5; 1; 4]) = 2.
Proof. reflexivity. Qed.

Example test_remove_on34:
  count 5 (remove_one 5 [2; 1; 5; 4; 5; 1; 4]) = 1.
Proof. reflexivity. Qed.

Fixpoint remove_all (v : nat) (s : bag) : bag :=
  match s with
    | nil => nil
    | h :: t =>
      match beq_nat v h with
        | true => remove_all v t
        | false => h :: remove_all v t
      end
  end.

Example test_remove_all1 : count 5 (remove_all 5 [2; 1; 5; 4; 1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all2 : count 6 (remove_all 5 [2; 1; 4; 1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all3 : count 4 (remove_all 5 [2; 1; 4; 5; 1; 4]) = 2.
Proof. reflexivity. Qed.
Example test_remove_all4 : count 5 (remove_all 5 [2; 1; 5; 4; 5; 1; 4; 5; 1; 4])
                                 = 0.
Proof. reflexivity. Qed.

Fixpoint subset (s1 : bag) (s2 : bag) : bool :=
  match s1 with
    | nil => true
    | h :: t =>
      match member h s2 with
        | true => subset t (remove_one h s2)
        | false => false
                     end
    end.

Example test_subset1 : subset [1; 2] [2; 1; 4; 1] = true.
Proof. reflexivity. Qed.
Example test_subset2 : subset [1; 2; 2] [2; 1; 4; 1] = false.
Proof. reflexivity. Qed.

Theorem nil_app : forall l : natlist,
    [] ++ l = l.
Proof. reflexivity. Qed.

Theorem tl_length_pred : forall l : natlist,
    pred (length l) = length (tl l).
Proof.
  intros l. destruct l as [| n l'].
  - (*l = nil*)
    simpl.
    reflexivity.
  - (*l = cons n l'*)
    simpl.
    reflexivity. 
Qed.

End NatList.
