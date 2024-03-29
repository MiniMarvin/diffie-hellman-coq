(*** Completeness proof for Diffie Hellma(DH)n Algorithm ***)
(*** To proof it we must first proof that ***)


Require Export Coq.Init.Nat.
Require Export Coq.Strings.String.
Require Export Coq.Strings.Ascii.

Require Export Coq.Numbers.Natural.Abstract.NDiv.

Require Export Coq.ZArith.Znat.
Require Export Coq.ZArith.ZArith_base.
Require Export Coq.ZArith.Zcomplements.
Require Export Coq.ZArith.Zdiv.
Require Export Coq.ZArith.ZArith.

Require Import Omega.

(*** 
    DH algorithm works basic as an Alice and Bob communication and they
    have some important numbers, they are:

    Alice's PublicKey (A)
    Bob's PublicKey (B)
    Alice's PrivateKey (a)
    Bob's PrivateKey (b)

    Also some other important numbers must exist to make it work:
    a Prime number (p) 
    a Base (g) 

    These numbers are not secret, and can be known by anyone in the network.
    P must be a prime number, and G is a Primitive root modulo.

    Those keys are used to compute other keys and then a shared secret:
    A = g^a mod p
    B = g^b mod p

    And then Alice sends it's public key to Bob, and vice versa, so they 
    compute a shared secret (shared seed) in both sides of the network:


    SharedSeed = B^a mod p
    SharedSeed = A^b mod p

    In notation the prime number may be also called as sharedPrime.
***)



(*** Helper functions****)
Print String.eqb. 

(** DH Functions **)
(** base ˆ exponent mod module **)
Fixpoint modExp (base : nat) (exponent : nat) (modulo : nat): nat :=
(pow base exponent) mod modulo.

Compute modExp 3 7 (10).

Fixpoint calculatePublicKey (private_key : nat) (sharedPrime : nat) (sharedBase : nat) : nat :=
  modExp sharedBase private_key sharedPrime.

Compute calculatePublicKey 3 7 (10).

Fixpoint calculateSharedSeed (private_key : nat) (public_key : nat) 
         (sharedPrime : nat) : nat :=
modExp public_key private_key sharedPrime.

Compute length "abc".



Fixpoint caesarCrypt (seed : nat) (s : string) : string :=
  match s with
  | EmptyString => EmptyString
  | String c s' => append (String (ascii_of_nat (((nat_of_ascii c) + seed))) EmptyString) (caesarCrypt seed s')
  end
.

Compute caesarCrypt 257 "hello world".

Print Z.of_nat.

Fixpoint moduloDiff256 (asciiNumber:nat) (seed:nat) : nat :=
  if seed =? 0 then
    asciiNumber
  else
    if asciiNumber <? seed then 
      if 256 <? (seed - asciiNumber) then
        seed + asciiNumber
      else 
        256 - seed + asciiNumber
    else
      asciiNumber - seed
.

Compute moduloDiff256 10 1200.


Lemma diffZ0 : forall (a:Z), Z.sub a 0 = a.
Proof.
  intros. induction a.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Theorem moduleDiff256_of_0: forall (asciiNumber:nat), moduloDiff256 asciiNumber 0 = asciiNumber mod 256. 
Proof.
  intros. 
  induction asciiNumber.
  - simpl. reflexivity.
  - unfold moduloDiff256. Search (Z.of_nat 0). 
Abort.

Definition getAscii (seed: nat) (c: ascii) : ascii :=
if seed <=? (nat_of_ascii c) then
  ascii_of_nat ((nat_of_ascii c) - seed)
else
  ascii_of_nat (256 - nat_of_ascii (ascii_of_nat (seed - (nat_of_ascii c))))
.



Fixpoint caesarDecrypt (seed : nat) (s: string) : string :=
  match s with
  | EmptyString => EmptyString
  | String c s' => append (String (getAscii seed c) EmptyString) (caesarDecrypt seed s')
  end
(** | String c s' => append (String (ascii_of_nat ((moduloDiff256 (nat_of_ascii c) seed))) EmptyString) (caesarDecrypt seed s') **)
.

Compute caesarDecrypt 257 "ifmmp!xpsme".

Compute caesarCrypt 513 "hello world".
Compute caesarDecrypt 513 "ifmmp!xpsme".

Theorem caesarCorrect:
    forall (message: string) (seed: nat), 
    (caesarDecrypt seed (caesarCrypt seed message)) = message.
Proof.
  intros.
  induction seed.
  - destruct message.
    + simpl. reflexivity.
    + simpl. Search (_ + 0). rewrite Nat.add_0_r. Search (_ - 0). 
    (**
      rewrite Nat.sub_0_r.
      rewrite ascii_nat_embedding. rewrite ascii_nat_embedding.
      rewrite decrypt0.
      assert(crypt0: caesarCrypt 0 message = message).
      {
          induction message.
              - simpl. reflexivity.
              - simpl. rewrite Nat.add_0_r. rewrite IHmessage. 
                + rewrite ascii_nat_embedding. reflexivity.
      }
      rewrite crypt0. reflexivity.

  - induction message.
      + simpl. reflexivity.
      + simpl.
        assert(H1: (ascii_of_nat (nat_of_ascii (ascii_of_nat (nat_of_ascii a + S seed)) - S seed)) = a).
        {
          Search(ascii_of_nat _). rewrite nat_ascii_embedding. 

          - rewrite Aux1. reflexivity.
          admit.
        }
        rewrite H1.
        assert(H2: (caesarDecrypt (S seed) (caesarCrypt (S seed) message)) = message). {
          simpl. rewrite IHmessage. 
            - reflexivity.
            - admit.
        }
        rewrite H2. reflexivity.
    **)
Abort.

Print string_dec.
Compute String.eqb "abc" "abc".
Compute String.eqb "abc" "acb".

Lemma caesarCrypt0 : 
  forall (message:string),
  caesarCrypt 0 message = message.
Proof.
  intros. 
  induction message.
  - simpl. reflexivity.
  - inversion IHmessage.
    + simpl. rewrite H.
      Search (_ + 0). rewrite Nat.add_0_r.
      rewrite H. Search (ascii_of_nat (nat_of_ascii _)).
      rewrite ascii_nat_embedding. reflexivity. 
Qed.

Lemma moduloDiff256_0_is_n : 
  forall (n:nat),
  moduloDiff256 n 0 = n.
Proof.
  intros. induction n.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Lemma getAscii_0_is_n : 
  forall (n:ascii),
  getAscii 0 n = n.
Proof.
  intros. unfold getAscii.
  simpl. Search(_ - 0). rewrite Nat.sub_0_r. rewrite ascii_nat_embedding. reflexivity.
Qed.

Lemma caesarDecrypt0 :
  forall (message:string),
  caesarDecrypt 0 message = message.
Proof.
  intros.
  induction message.
  - simpl. reflexivity.
  - simpl. rewrite getAscii_0_is_n. rewrite IHmessage. reflexivity.
Qed.

Theorem caesarBoolCorret:
  forall (message: string) (seed: nat),
  (String.eqb (caesarDecrypt seed (caesarCrypt seed message)) message) = true.
Proof.
intros.
induction seed.
  - rewrite caesarCrypt0. rewrite caesarDecrypt0. Search(String.eqb _ _).
    rewrite String.eqb_refl.
    reflexivity.
  - 
Abort.

(*** Check proof for append in lists ***)
Lemma caesarCryptUnfold :
  forall (seed: nat) (m1: string) (m2:string),
  caesarCrypt seed (append m1 m2) = 
  append (caesarCrypt seed m1) (caesarCrypt seed m2).
Proof.
  intros.
  induction m1.
  - simpl. reflexivity.
  - intros. simpl. rewrite <- IHm1. reflexivity.
Qed.

(*** Check proof for append in lists ***)
Lemma caesarDecryptUnfold :
  forall (seed: nat) (m1: string) (m2: string),
  caesarDecrypt seed (append m1 m2) = 
  append (caesarDecrypt seed m1) (caesarDecrypt seed m2).
Proof.
  intros.
  induction m1.
  - simpl. reflexivity.
  - simpl. rewrite <- IHm1. reflexivity.
Qed.



Search (String _ _).
Lemma concatToAppend :
  forall (a:ascii) (s:string),
  String a s = append (String a EmptyString) s.
Proof.
  intros. simpl.
  reflexivity.
Qed.

Lemma getAsciiSimpl : 
  forall (a: ascii) (seed: nat),
  getAscii seed (ascii_of_nat (nat_of_ascii a + seed)) = a.
Proof.
  intros. simpl.
  induction seed.
  - Search (_ + 0). rewrite Nat.add_0_r.
    Search (ascii_of_nat (nat_of_ascii _)). rewrite ascii_nat_embedding.
    unfold getAscii. simpl. Search (_ - 0). rewrite Nat.sub_0_r.
    rewrite ascii_nat_embedding. reflexivity.
  - Search (_ + S _). rewrite <- Nat.add_1_r. Search (_ + (_ + _)).
    rewrite Nat.add_assoc. unfold getAscii. 
    destruct (seed + 1 <=? nat_of_ascii (ascii_of_nat (nat_of_ascii a + seed + 1))) eqn:H.
    + simpl.
Admitted.

Theorem caesarCorrect:
  forall (message: string) (seed: nat),
  (caesarDecrypt seed (caesarCrypt seed message)) = message.
Proof.
  intros.

  (** Caminho sobre seed **)
  induction seed.
  - rewrite caesarCrypt0. rewrite caesarDecrypt0. reflexivity.
  - 

  (** Caminho sobre message **)
  induction message.
  - simpl. reflexivity.
  - rewrite concatToAppend. rewrite caesarCryptUnfold.
    rewrite caesarDecryptUnfold. rewrite IHmessage.
    Check IHmessage. Compute (String a EmptyString).
    simpl in IHmessage. 
    (** remember (String a "") as message1.
    subst. **)
    assert(caesarDecrypt seed (caesarCrypt seed (String a "")) = String a ""). {
      unfold caesarCrypt. simpl. rewrite getAsciiSimpl. reflexivity.
    }
    rewrite H. reflexivity.
Qed.


(*** Extra: Proof for any symmetric encryption ***)
Fixpoint symmetricCrypt (seed : nat) (msg_toSend: string) : string.
Proof. Admitted.

Fixpoint symmetricDecript (seed : nat) (msg_received: string) : string.
Proof. Admitted.

(*** Extra: Proof for any symmetric encryption ***)

Theorem equalKeys :
    forall (private_k1:nat) (private_k2:nat) (gen:nat) (base:nat), 
    (calculateSharedSeed private_k1 
      (calculatePublicKey private_k2 base gen) base gen) = 
    (calculateSharedSeed private_k2 
      (calculatePublicKey private_k1 base gen) base gen).
Proof. Admitted.

Theorem DHCorrect:
    forall (message: string) (seed: nat), 
    (symmetricDecript seed (symmetricCrypt seed message)) = message.
Proof. Admitted.

Theorem Encrypt_Complete: 
    forall (message: string) (seed: nat), caesarCrypt seed message <> message.
Proof. Admitted.

(*** https://github.com/coq-contribs/rsa/blob/master/Rsa.v ***)


(*** Extras: Rationalize over DiffieHellman protocol implementations using 
Protocol Composition Logic, as defined in this paper from stanford university:
http://seclab.stanford.edu/pcl/papers/rdm-tgc07.pdf ***)


Theorem decrypt0: forall (message:string), caesarDecrypt 0 message = message.
Proof.
intros.
induction message.
  - simpl. reflexivity.
  - simpl. unfold moduloDiff256.
rewrite Nat.sub_0_r. rewrite IHmessage. rewrite ascii_nat_embedding. reflexivity.
Qed.

Theorem Aux1: forall (a:ascii) (seed:nat), ascii_of_nat (nat_of_ascii a + S seed - S seed) = a. Admitted.
