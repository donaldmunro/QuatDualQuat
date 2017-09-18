QuatDualQuat Mathematica Package for Quaternions and Dual Quaternions
======================================================================

# Description

Q&D (Quick and Dirty) Mathematica package for Quaternions and Dual Quaternions prompted mostly by not being
able to easily use symbolics in the official Quaternion package. This package is simpler and does not
attempt to provide a Quaternion object, instead it just uses plain lists to represent quaternions and nested
list of lists for dual quaternions in pretty much the same way you would implement a C library ie passing in
parameters and returning quaternions/dual quaternions etc. It does however work with symbolics and the use of
plain lists means things like scalar multiplication and norms can be provided by normal Mathematica operations.

**Disclaimer** I haven't done much (any) Mathematica programming so the code could probably be improved upon


# Functions

## Quaternions

_QCreate[w, x, y, z],  QCreate[vec4], QCreate[scalar, vec3]_ creates a quaternion as a 4-list.

_QW[Q]_ returns the non-imaginary (w) part of Q.

_QV[Q]_ returns the imaginary ({x, y, z}) part of Q as a 3-list.

_QAdd[Q1, Q2]_ returns the quaternion as a 4-list representing the addition of Q1 and Q2.

_QTimes[Q1, Q2]_ returns the quaternion as a 4-list representing the (non-commutative) multiplication of Q1 and Q2.

_QSandwich[Q, P]_ returns the quaternion as a 4-list representing the multiplication of quaternion Q, the 3-list P and the conjugate of Q.

_QRotate[[p, axis, angle]_ returns the quaternion as a 4-list representing the rotation angle around axis (3-list) of a point p (3-list).

_QConjugate[Q]_ returns the quaternion as a 4-list representing the conjugate of Q.

_QInverse[Q]_ returns the quaternion as a 4-list representing the inverse of Q.

_QCollect[Q, var]_ returns a list comprising a scalar/variable and a 4-list representing a quaternion. The return values are the result of calling Collect[Q, var] on the quaternion components. If Collect cannot separate var from all the expressions in Q then 1 is returned as the first element of the return list].
**Note** This could possibly be implemented more cleanly by returning var * Q, however Mathematica evaluates the return
and distributes var again. [http://community.wolfram.com/groups/-/m/t/858630](http://community.wolfram.com/groups/-/m/t/858630) 
provides some sort of work around (for Mathematica 10 and up) but it doesn't seem to work for function returns.

_QString[Q, format]_ returns a formatted string representation of the quaternion Q using format (defaults to TraditionalForm).

_QStringCollect[Q, var, format]_ returns a formatted string representation of the quaternion Q using format (defaults to TraditionalForm) after applying Collect(Q. var).

## Dual Quaternions

_DQCreate[w1, x1, y1, z1, w2, x2, y2, z2], DQCreate[Q1, Q2]_ creates a Dual Quaternion
 as a nested list of two quaternions with the 2nd quaternion representing the dual number quaternion.

_DQReal[DQ]_ returns a 4-list representing a quaternion which is the real part of the dual quaternion DQ.

_DQRealScalar[DQ]_ returns the real part of the quaternion which is the real part of the dual quaternion DQ.

_DQRealVector[DQ]_ returns the imaginary part of the quaternion which is the real part of the dual quaternion DQ.

_DQDual[DQ]_ returns a 4-list representing a quaternion which is the dual part of the dual quaternion DQ.

_DQDualScalar[DQ]_ returns the real part of the quaternion which is the dual part of the dual quaternion DQ.

_DQDualVector[DQ]_ returns the imaginary part of the quaternion which is the dual part of the dual quaternion DQ.

_DQAdd[DQ1, DQ2]_ returns a nested list of two quaternions representing the dual quaternion resulting from adding DQ1 and DQ2.

_DQTimes[DQ1, DQ2]_ returns the dual quaternion as a nested list of two quaternions representing the (non-commutative) multiplication of DQ1 and DQ2.

_DQSandwich[DQ, P]_ returns the dual quaternion as a nested list of two quaternions representing
 the multiplication of DQ, the 3-list P and the conjugate of DQ.

_DQRigidTransform[p_, raxis_, theta_, t_]_ returns the dual quaternion as a nested list of two quaternions representing  the rotation of point p (3-list) of angle theta around axis (3-list) followed by translation t (3-list). 

_DQConjugate[DQ]_ returns the dual quaternion as a nested list of two quaternions representing the first conjugate of DQ (p* + eq*).

_DQConjugate2[DQ]_ returns the dual quaternion as a nested list of two quaternions representing the second conjugate of DQ (p* - eq*).

_DQNorm[DQ]_ returns the norm of DQ.

_DQString[DQ, format]_ returns a formatted string representation of the dual quaternion DQ using format (defaults to TraditionalForm).

_DQStringCollect[Q, var, format]_ returns a formatted string representation of the dual quaternion DQ using format (defaults to TraditionalForm) after applying Collect(Q. var) to both components of DQ.
