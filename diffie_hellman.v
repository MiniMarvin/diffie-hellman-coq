(*** Completeness proof for Diffie Hellma(DH)n Algorithm ***)
(*** To proof it we must first proof that ***)

Require export Coq.ZArith.Zdiv.

(*** 
    DH algorithm works basic as an Alice and Bob communication and they
    have some important numbers, they are:
    
    Alice's PublicKey, Bob's PublicKey
    Alice's PrivateKey, Bob's PrivateKey
    
    Those keys are used to compute the final key with the equation:
    
    SharedSeed = g^
***)



(*** Helper functions****)
Fixpoint isEqual (a: String) (b: String) : Bool

(** DH Functions **)
Fixpoint modExp (b : nat) (e : nat) (m : nat): nat.

Fixpoint calculatePublicKey (private_k : nat, base : nat, gen : nat) : nat.

Fixpoint calculateSharedSeed (private_k : nat, public_k : nat, base : nat, gen : nat) : nat.

Fixpoint caesarCrypt (seed : .nat, msg_toSend: String) : String.

Fixpoint caesarDecript (seed : .nat, msg_received: String) : String.


(*** Extra: Proof for any symmetric encryption ***)
Fixpoint symmetricCrypt (seed : .nat, msg_toSend: String) : String.

Fixpoint symmetricDecript (seed : .nat, msg_received: String) : String.

(*** Extra: Proof for any symmetric encryption ***)

Theorem equalKeys:
    forall (private_k1:nat) (private_k2:nat) (gen:nat) (base:nat), 
    calculateSharedSeed(private_k1, calculatePublicKey(private_k2, base, gen), base, gen) = calculateSharedSeed(private_k2, calculatePublicKey(private_k1, base, gen), base, gen) 

Theorem DHCorrect:
    forall message:String, (symmetricDecript( symmetricCrypt (message) )) = message


Theorem Encrypt_Complete: 
    forall message: String, caesarCrypt message

(*** https://github.com/coq-contribs/rsa/blob/master/Rsa.v ***)


(*** Extras: Rationalize over DiffieHellman protocol implementations using 
Protocol Composition Logic, as defined in this paper from stanford university:
http://seclab.stanford.edu/pcl/papers/rdm-tgc07.pdf ***)


