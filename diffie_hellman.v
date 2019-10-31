(*** Completeness proof for Diffie Hellma(DH)n Algorithm ***)
(*** To proof it we must first proof that ***)

Require Export Coq.ZArith.Zdiv.
Require Export Coq.Init.Nat.
Require Export Coq.Strings.String.
Require Export Coq.Numbers.Natural.Abstract.NDiv.

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
Fixpoint isEqual (a: string) (b: string) : bool.
Proof. Admitted.

(** DH Functions **)
Fixpoint modExp (base : nat) (exponent : nat) (modulo : nat): nat :=
(pow base exponent) mod modulo.

Compute modExp 3 7 (10).

Fixpoint calculatePublicKey (private_k : nat) (sharedPrime : nat) (gen : nat) : nat :=
  modExp gen private_k sharedPrime.

Fixpoint calculateSharedSeed (private_k : nat) (public_k : nat) 
         (sharedPrime : nat) : nat :=
modExp public_k private_k sharedPrime.

Fixpoint caesarCrypt (seed : nat) (msg_toSend: string) : string.
Proof. Admitted.

Fixpoint caesarDecript (seed : nat) (msg_received: string) : string.
Proof. Admitted.

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


