(* ::Package:: *)

(* :Title: Simple Quaternion/Dual Quaternion Package*)

(* :Author: Donald Munro *)

(* :Version: Mathematica 10.0 *)

(* :Package Version: 0.1 *)

(* :Keywords:
    quaternions, dual quaternions
*)

BeginPackage["SimpleQuatDualQuat`"]

If[Not@ValueQ[QCreate::usage],QCreate::usage = "QCreate[w, x, y, z],  QCreate[vec4], QCreate[scalar, vec3] creates a quaternion as a 4-list."];

If[Not@ValueQ[QW::usage],QW::usage = "QW[Q] returns the non-imaginary (w) part of Q."];

If[Not@ValueQ[QV::usage],QV::usage = "QV[Q] returns the imaginary ({x, y, z}) part of Q as a 3-list."];

If[Not@ValueQ[QAdd::usage],QAdd::usage = "QAdd[Q1, Q2] returns the quaternion as a 4-list representing the addition of Q1 and Q2."];

If[Not@ValueQ[QTimes::usage],QTimes::usage = "QTimes[Q1, Q2] returns the quaternion as a 4-list representing the (non-commutative) multiplication of Q1 and Q2."];

If[Not@ValueQ[QSandwich::usage],QSandwich::usage = "QSandwich[Q, P] returns the quaternion as a 4-list representing the multiplication of quaternion Q, the 3-list P and the conjugate of Q."];

If[Not@ValueQ[QRotate::usage],QRotate::usage = "QRotate[[p, axis, angle] returns the quaternion as a 4-list representing the rotation angle around axis (3-list) of a point p (3-list)."];

If[Not@ValueQ[QRotateBetween::usage],QRotateBetween::usage = "QRotateBetween[v1, v2, simplify:True] returns the quaternion as a 4-list representing the shortest arc rotation between two vectors v1 (3-list) and v2 (3-list). simplify specifies whether to run FullSimplify on the quaternion components."];

If[Not@ValueQ[QConjugate::usage],QConjugate::usage = "QConjugate[Q] returns the quaternion as a 4-list representing the conjugate of Q."];

If[Not@ValueQ[QInverse::usage],QInverse::usage = "QInverse[Q] returns the quaternion as a 4-list representing the inverse of Q."];

If[Not@ValueQ[QCollect::usage],QCollect::usage = "QCollect[Q, var] returns a list comprising a scalar/variable and a 4-list representing a quaternion.\
 The return values are the result of calling Collect[Q, var] on the quaternion components. If Collect cannot separate var from all the expressions in Q\
  then 1 is returned as the first element of the return list"];

If[Not@ValueQ[QString::usage],QString::usage = "QString[Q, format] returns a formatted string representation of the quaternion Q using format (defaults to TraditionalForm)."];

If[Not@ValueQ[QStringCollect::usage],QStringCollect::usage = "QStringCollect[Q, var, format] returns a formatted string representation of the quaternion Q using format (defaults to TraditionalForm)\
 after applying Collect(Q. var)."];

If[Not@ValueQ[DQCreate::usage],DQCreate::usage = "DQCreate[w1, x1, y1, z1, w2, x2, y2, z2], DQCreate[Q1, Q2] creates a Dual Quaternion\
 as a nested list of two quaternions with the 2nd quaternion representing the dual number quaternion."];

If[Not@ValueQ[DQReal::usage],DQReal::usage = "DQReal[DQ] returns a 4-list representing a quaternion which is the real part of the dual quaternion DQ."];

If[Not@ValueQ[DQRealScalar::usage],DQRealScalar::usage = "DQRealScalar[DQ] returns the real part of the quaternion which is the real part of the dual quaternion DQ."];

If[Not@ValueQ[DQRealVector::usage],DQRealVector::usage = "DQRealVector[DQ] returns the imaginary part of the quaternion which is the real part of the dual quaternion DQ."];

If[Not@ValueQ[DQDual::usage],DQDual::usage = "DQDual[DQ] returns a 4-list representing a quaternion which is the dual part of the dual quaternion DQ."];

If[Not@ValueQ[DQDualScalar::usage],DQDualScalar::usage = "DQDualScalar[DQ] returns the real part of the quaternion which is the dual part of the dual quaternion DQ."];

If[Not@ValueQ[DQDualVector::usage],DQDualVector::usage = "DQDualVector[DQ] returns the imaginary part of the quaternion which is the dual part of the dual quaternion DQ."];

If[Not@ValueQ[DQAdd::usage],DQAdd::usage = "DQAdd[DQ1, DQ2] returns a nested list of two quaternions representing the dual quaternion resulting from adding DQ1 and DQ2."];

If[Not@ValueQ[DQTimes::usage],DQTimes::usage = "DQTimes[DQ1, DQ2] returns the dual quaternion as a nested list of two quaternions representing the (non-commutative) multiplication of DQ1 and DQ2."];

If[Not@ValueQ[DQSandwich::usage],DQSandwich::usage = "DQSandwich[DQ, P] returns the dual quaternion as a nested list of two quaternions representing\
 the multiplication of DQ, the 3-list P and the conjugate of DQ."];

If[Not@ValueQ[DQRigidTransform::usage],DQRigidTransform::usage = "DQRigidTransform[p_, raxis_, theta_, t_] returns the dual quaternion as a nested list of two quaternions representing\
 the rotation of point p (3-list) of angle theta around axis (3-list) followed by translation t (3-list). "];

If[Not@ValueQ[DQConjugate::usage],DQConjugate::usage = "DQConjugate[DQ] returns the dual quaternion as a nested list of two quaternions representing the first conjugate of DQ (p* + eq*)"];

If[Not@ValueQ[DQConjugate2::usage],DQConjugate2::usage = "DQConjugate2[DQ] returns the dual quaternion as a nested list of two quaternions representing the second conjugate of DQ (p* - eq*)"];

If[Not@ValueQ[DQNorm::usage],DQNorm::usage = "DQNorm[DQ] returns the norm of DQ."];

If[Not@ValueQ[DQString::usage],DQString::usage = "DQString[DQ, format] returns a formatted string representation of the dual quaternion DQ using format (defaults to TraditionalForm)."];

If[Not@ValueQ[DQStringCollect::usage],DQStringCollect::usage = "DQStringCollect[Q, var, format] returns a formatted string representation of the dual quaternion DQ using format (defaults to TraditionalForm)\
 after applying Collect(Q. var) to both components of DQ."];

(*================================================================================================================*)

Begin["`Private`"]

QCreate[w_, x_, y_, z_] := { w, x, y, z }
QCreate[vec4_ /; Length[vec4] == 4] := vec4
QCreate[scalar_, vec3_ /; Length[vec3] == 3] := Flatten[{ scalar, vec3 }]

QW[Q_] := Q[[1]]

QV[Q_] := Q[[2 ;; 4]]

QAdd[Q1_, Q2_] := Q1 + Q2

QTimes[Q1_, Q2_] := Flatten[{ Q1[[1]]*Q2[[1]] - Dot[Q1[[2 ;; 4]], Q2[[2 ;; 4]]], Q1[[1]]*Q2[[2 ;; 4]] + Q2[[1]]*Q1[[2 ;; 4]] + Cross[Q1[[2 ;; 4]], Q2[[2 ;; 4]]]}]

QSandwich[Q_, P_] := QTimes[QTimes[Q, P], QConjugate[Q]]

QRotate[p_, axis_, angle_] := { 0, (1 - Cos[angle]) Dot[axis, p] axis + Cos[angle] p + Sin[angle] Cross[axis, p] }

QRotateBetween[v1_, v2_, simpl_:True] := Module[{c,d, Q, qw, qxyz},
(
   c = Cross[v1, v2];
   d = Dot[v1, v2];
   qw = Sqrt[2(1 + d)]/2;
   qxyz = c/Sqrt[2(1 + d)];
   If[simpl, Q = QCreate[FullSimplify[qw], FullSimplify[qxyz[[1]]], FullSimplify[qxyz[[2]]], FullSimplify[qxyz[[3]]]], 
   Q = QCreate[qw, qxyz[[1]], qxyz[[2]], qxyz[[3]]]];
   Return[Q]
)]

QConjugate[Q_] := Flatten[{ Q[[1]], -1*Q[[2 ;; 4]] }]

QInverse[Q_] := QConjugate[Q] / Norm[Q]^2

QCollect[Q_, scalar_] := Module[{expr, count, q},
(count = 0;
q = {};
For[i =1, i<5, i++,
(
  expr = Collect[Q[[i]], scalar];
  If[Length[expr] > 0 && expr[[1]] == scalar,
 ( count++; If[Length[expr] > 1,AppendTo[q, expr[[2]]], AppendTo[q, 1]]),
If[Length[expr] == 0 && expr == scalar, (count++; AppendTo[q, 1]),
Nothing]
])
];
If [count <4,{1,Q},{scalar,q}]
)]

QString[Q_,format_:TraditionalForm] := "[" <> ToString[Q[[1]], format] <> ", (" <> ToString[Q[[2]], format] <>", "  <>  ToString[Q[[3]], format] <> ", " <> ToString[Q[[4]], format] <> ")]"

QStringCollect[Q_, scalar_,format_:TraditionalForm] := Module[{ll, s},
(
ll = QCollect[Q, scalar];
s = "";
If[ll[[1]] == scalar,
Return[ToString[ll[[1]], format]<>QString[ll[[2]], format]],
Return[QString[ll[[2]], format]]];
)
]

DQCreate[w1_, x1_, y1_, z1_, w2_, x2_, y2_, z2_] := { { w1, x1, y1, z1 },{ w2, x2, y2, z2 } }
DQCreate[Q1_ /; Length[Q1] == 4, Q2_ /; Length[Q2] == 4] := { Q1, Q2 }

DQReal[DQ_] := DQ[[1]]
DQRealScalar[DQ_] := DQ[[1]][[1]]
DQRealVector[DQ_] := DQ[[1]][[2 ;; 4]]
DQDual[DQ_] := DQ[[2]]
DQDualScalar[DQ_] := DQ[[2]][[1]]
DQDualVector[DQ_] := DQ[[2]][[2 ;; 4]]

DQAdd[DQ1_, DQ2_] := { Part[DQ1, 1] +Part[DQ2, 1],  Part[DQ1, 2] + Part[DQ2, 2]}

DQTimes[DQ1_, DQ2_] :=
{ QTimes[DQ1[[1]],DQ2[[1]]],   QAdd[QTimes[DQ1[[1]],DQ2[[2]]] , QTimes[DQ1[[2]],DQ2[[1]]]]
}

DQSandwich[DQ_, p_] := Module[{DP},
(
DP = DQCreate[QCreate[1, 0, 0, 0], QCreate[0, p]];
Return[DQTimes[DQTimes[DQ, DP], DQConjugate2[DQ]]];
)
]

DQRigidTransform[p_, raxis_, theta_, t_] := Module[{Qt, Qp, QR, QRpR},
(
   Qt = QCreate[0, t];
   Qp = QCreate[0, p];
   QR = QCreate[Cos[theta/2], Sin[theta/2] raxis[[1]], Sin[theta/2] raxis[[2]], Sin[theta/2] raxis[[3]]];
   QRpR = QTimes[QTimes[QR, Qp], QConjugate[QR]];
   Return[DQCreate[QCreate[1, 0, 0, 0], QCreate[0, QV[QAdd[QRpR, Qt]]]]]
(*
   QR = QCreate[Cos[theta/2], Sin[theta/2]Normalize[raxis]];
   QT = QCreate[0, t];
   DQ = DQCreate[QR, 1/2 QTimes[QT, QR]];
   Return[DQSandwich[DQ, p]];
*)   
)]

DQConjugate[DQ_] := { QConjugate[DQ[[1]]], QConjugate[DQ[[2]]] }

DQConjugate2[DQ_] := { QConjugate[DQ[[1]]], -1*QConjugate[DQ[[2]]] }

DQDualConjugate[DQ_] := { DQ[[1]], -1*DQ[[2]]}

DQNorm[DQ_] := Sqrt[DQTimes[DQ, DQConjugate[DQ]]]

DQString[DQ_,format_:TraditionalForm] := QString[DQ[[1]], format]<>" + \[Epsilon]"<>QString[DQ[[2]], format]

DQStringCollect[DQ_, scalar_,format_:TraditionalForm] := QStringCollect[DQ[[1]], scalar, format]<>" + "<>"\[Epsilon]"QStringCollect[DQ[[2]], scalar, format]

End[]  (* Private*)

EndPackage[] 



