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

Theorem app_assoc : forall l1 l2 l3 : natlist,
    (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
  intros l1 l2 l3. induction l1 as [ | n l1' IHl1'].
  - (*l1 = nil*)
    simpl.
    reflexivity.
  - (*l1 = cons n l1'*)
    simpl.
    rewrite -> IHl1'.
    reflexivity.
Qed.

Fixpoint rev (l : natlist) : natlist :=
  match l with
    | nil => nil
    | h :: t => rev t ++ [h]
  end.

Example test_rev1 : rev [1; 2; 3] = [3; 2; 1].
Proof. simpl. reflexivity. Qed.
Example test_rev2 : rev nil = nil.
Proof. simpl. reflexivity. Qed.

Theorem rev_length_firsttry : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros l. induction l as [| n l' IHl'].
  - (*l = nil*)
    simpl.
    reflexivity.
 - (*l = n :: l'*)
   simpl.
   (*Now we seem to be stuck : the goal is an equality involving ++,
   but we don't have any useful equations in either the immediate
   context or in the global environment! We can make a little progress
   by using the IH to rewrite the goal...*)
   rewrite <- IHl'.
   (*... but now we can't go any further.*)
Abort.

Theorem app_length : forall l1 l2 : natlist,
  length (l1 ++ l2) = length l1 + length l2.
Proof.
  intros l1 l2. induction l1 as [| n l1' IHl'].
  - (*l1 = nil*)
    simpl.
    reflexivity.
  - (*l1 = cons n l1'*)
    simpl.
    rewrite <- IHl'.
    reflexivity.
Qed.

Theorem rev_length : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros l. induction l as [| n l' IHl'].
  - (*l = nil*)
    simpl.
    reflexivity.
  - (*l = n :: l'*)
    simpl.
    rewrite -> app_length, plus_comm.
    simpl.
    rewrite -> IHl'.
    reflexivity.
Qed.

Theorem rev_cons_as_app : forall (h : nat) (l : natlist),
    rev (h :: l) = rev l ++ [h].
Proof.
      reflexivity.
Qed.

Theorem app_nil_r : forall l : natlist,
  l ++ [] = l.
Proof.
  intros l.
  induction l as [| h l' IHl'].
  - (*l = nil*)
    simpl.
    reflexivity.
  - (*l = h :: l'*)
    assert (H : forall m : natlist, 
               (h :: l') ++ m = h :: (l' ++ m)). { reflexivity. }
    rewrite -> H.
    rewrite -> IHl'.
    reflexivity.
Qed.

Theorem rev_app_distr : forall l1 l2 : natlist,
  rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros l1 l2.
  induction l1 as [ | h l1' IHl1' ].
  - (*l1 = nil*)
    simpl.
    rewrite -> app_nil_r.
    reflexivity.
  - (*l1 = h :: l1'*)
    assert (H : rev ((h :: l1') ++ l2) = rev (l1' ++ l2) ++ [h]). {
      reflexivity.
    }
    rewrite -> H.
    rewrite -> IHl1'.
    rewrite -> rev_cons_as_app.
    rewrite -> app_assoc.
    reflexivity.
Qed.

Theorem rev_involutive : forall l : natlist,
  rev (rev l) = l.
Proof.
  intros l.
  induction l as [| h l' IHl'].
  - (*l = nil*)
    simpl.
    reflexivity.
  - (*l = h :: l'*)
    rewrite -> rev_cons_as_app.
    rewrite -> rev_app_distr.
    rewrite -> IHl'.
    simpl.
    reflexivity.
Qed.
    
Theorem app_assoc4 : forall l1 l2 l3 l4 : natlist,
  l1 ++ (l2 ++ (l3 ++ l4)) = ((l1 ++ l2) ++ l3) ++ l4.
Proof.
  intros l1 l2 l3 l4. induction l1 as [ | n l1' IHl1'].
  - (*l1 = nil*)
    rewrite -> app_assoc.
    reflexivity.
  - (*l1 = h :: l1'*)
    assert (H : forall (h : nat) (l m : natlist), 
               (h :: l) ++ m = h :: (l ++ m)). { reflexivity. }
    rewrite -> H, IHl1'.
    reflexivity.
Qed.

Lemma nonzeros_app : forall l1 l2 : natlist,
  nonzeros (l1 ++ l2) = (nonzeros l1) ++ (nonzeros l2).
Proof.
  intros l1 l2.
  induction l1 as [ | h l1' IHl1' ].
  - (*l1 = nil*)
    reflexivity.
  - assert (H : forall l : natlist, nonzeros (0 :: l) = nonzeros l).
    { reflexivity. }
    assert (H' : forall l m : natlist, 
               nonzeros ((0 :: l) ++ m) = nonzeros (l ++ m)). 
    {
      reflexivity.
    }
    { 
      destruct h.
      {
      rewrite -> H, H'.
      rewrite -> IHl1'.
      simpl.
      reflexivity. 
      }
      {
        simpl.
        rewrite -> IHl1'.
        reflexivity.
      }
    }
Qed.

Fixpoint beq_natlist (l1 l2 : natlist) : bool :=
  match l1 with
    | nil => 
      match l2 with
        | nil => true
        | _ => false
      end
    | (h1 :: t1) =>
      match l2 with
        | nil => false
        | (h2 :: t2) =>
        andb (beq_nat h1 h2) (beq_natlist t1 t2)
      end
  end.

Example test_beq_natlist1 : 
  (beq_natlist nil nil = true).
Proof. reflexivity. Qed.

Example test_beq_natlist2 :
  beq_natlist [1; 2; 3] [1; 2; 3] = true.
Proof. reflexivity. Qed.

Example test_beq_natlist3 :
  beq_natlist [1; 2; 3] [1; 2; 4] = false.
Proof. reflexivity. Qed.

Theorem beq_natlist_refl : forall l : natlist,
    true = beq_natlist l l.
Proof.
  intros l.
  induction l as [ | h l' IHl'].
  - (*l = nil*)
    reflexivity.
  - (*l = h :: l' *)
    simpl.
    rewrite <- IHl'.
    assert (H : forall n : nat, true = beq_nat n n).
    { intros n. induction n as [ | n' IHn'].
      - (*n = 0*)
        reflexivity.
      - (*n = S n'*)
        rewrite -> IHn'.
        reflexivity.
    }
    rewrite <- H.
    reflexivity.
Qed.

Theorem count_empty : forall (l : nat),
    count l [] = 0.
Proof.
  simpl.
  reflexivity.
Qed.

Theorem count_member_nonzero : forall s : bag,
    leb 1 (count 1 (1 :: s)) = true.
Proof.
  simpl.
 reflexivity.
Qed.

Theorem ble_n_Sn : forall n,
  leb n (S n) = true.
Proof.
  intros n. induction n as [| n' IHn'].
  - (* 0 *)
    simpl. reflexivity.
  - (* S n' *)
    simpl. rewrite IHn'. reflexivity. Qed.

Theorem remove_decreases_count : forall (s : bag),
  leb (count 0 (remove_one 0 s)) (count 0 s) = true.
Proof.
  intros s. induction s as [ | h t IHs].
  - (*nil*)
    simpl.
    reflexivity.
  - (*cons n t*)
    { destruct h as [| h'].
      {(*h = 0*)
        simpl.
        rewrite -> ble_n_Sn.
        reflexivity.
      }
      {(*h = S h'*)
        simpl.
        rewrite -> IHs.
        reflexivity.
      }
    }
Qed.

Inductive natoption : Type :=
  | Some : nat -> natoption
  | None : natoption.

Fixpoint nth_error (l : natlist) (n : nat) : natoption :=
  match l with
    | nil => None
    | a :: l' => match beq_nat n 0 with
                   | true => Some a
                   | false => nth_error l' (pred n)
                 end
  end.

Example test_nth_error1 : nth_error [4; 5; 6; 7] 0 = Some 4.
Proof. reflexivity. Qed.

Example test_nth_error2 : nth_error [4; 5; 6; 7] 3 = Some 7.
Proof. reflexivity. Qed.

Example test_nth_error3 : nth_error [4; 5; 6; 7] 9 = None.
Proof. reflexivity. Qed.

Fixpoint nth_error' (l : natlist) (n : nat) : natoption :=
  match l with
    | nil => None
    | a :: l' => if beq_nat n 0 then Some a
                 else nth_error' l' (pred n)
  end.

Definition option_elim (d : nat) (o : natoption) : nat :=
  match o with 
    | Some n' => n'
    | None => d
  end.

Definition hd_error (l : natlist) : natoption :=
  match l with
    | nil => None
    | (h :: _) => Some h
end.

Example test_hd_error1 : hd_error [] = None.
Proof. reflexivity. Qed.
Example test_hd_error2 : hd_error [1] = Some 1.
Proof. reflexivity. Qed.
Example test_hd_error3 : hd_error [5; 6] = Some 5.
Proof. reflexivity. Qed.

Theorem option_elim_hd : forall (l : natlist) (default : nat),
  hd default l = option_elim default (hd_error l).
Proof.
  intros l default.
  destruct l as [| n l'].
  - (*l = nil*)
    reflexivity.
  - (*l = n :: l'*)
    reflexivity.
Qed.


End NatList.

Inductive id : Type :=
  | Id : nat -> id.

Definition beq_id (x1 x2 : id) :=
  match (x1, x2) with
    | (Id n1, Id n2) => beq_nat n1 n2
  end.


Theorem beq_nat_refl : forall n,
    true = beq_nat n n.
Proof.
  intros n. induction n as [| n' IHn].
  - (*n = 0*)reflexivity.
  - (*n = S n'*)
    simpl.
    rewrite <- IHn.
    reflexivity.
Qed.

Theorem beq_id_refl : forall x,
    true = beq_id x x.
Proof.
  intros x.
  destruct x as [x'].
  assert (H : forall (x1 x2 : nat), beq_id (Id x1) (Id x2) = beq_nat x1 x2).
  { reflexivity. }
  rewrite -> H.
  rewrite <- beq_nat_refl.
  reflexivity.
Qed.

Module PartialMap.
Export NatList.

Inductive partial_map : Type :=
  | empty : partial_map
  | record : id -> nat -> partial_map -> partial_map.

Definition update (d : partial_map)(x : id) (value : nat) :=
  record x value d.

Fixpoint find (x : id) (d : partial_map) : natoption :=
  match d with
    | empty => None
    | record y v d' => if beq_id x y
                       then Some v
                       else find x d'
  end.

Theorem update_eq :
  forall (d : partial_map) (x : id) (v : nat),
    find x (update d x v) = Some v.
Proof.
  intros d x v.
  simpl.
  rewrite <- beq_id_refl.
  reflexivity.
Qed.

Theorem update_neq :
  forall (d : partial_map) (x y : id) (o : nat),
    beq_id x y = false -> find x (update d y o) = find x d.
Proof.
  intros .
  simpl.
  rewrite -> H.  
  reflexivity.
Qed.

end PartialMap.